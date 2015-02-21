{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Cartel (empty)
import qualified Cartel as Cartel
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mapM_, liftM, when)
import Control.Exception (catch, throw, SomeException)
import Crypto.Hash (hash, digestToHexByteString, Digest, MD5)
import Data.Aeson (Value(..), ToJSON(..), FromJSON(..), encode, decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Generics.Uniplate.Data (universeBi)
import Data.Key (mapWithKeyM_)
import Data.List (intercalate, find, filter, nub)
import qualified Data.Map.Strict as Map
import Data.Map.Lazy (foldrWithKey)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Time.LocalTime (getZonedTime)
import qualified Data.Version as V
import Distribution.Hackage.DB (readHackage, hackagePath)
import Distribution.PackageDescription (condLibrary, condTreeData, exposedModules)
import Distribution.Text (display)
import Distribution.Version (versionBranch)
import GHC.Generics (Generic)
import Language.Haskell.Exts (fromParseResult, parseModule)
import Language.Haskell.Exts.Syntax (Module(..), ModuleName(..), ImportDecl(..), QName(..), Name(..), Exp(..), SrcLoc(..))
import Language.Haskell.Exts.Pretty (prettyPrint)
import System.Console.CmdTheLine (Term, TermInfo(..), OptInfo(..), PosInfo(..), flag, value, pos, posAny, optInfo, posInfo, defTI)
import System.Console.CmdTheLine.Term (run)
import System.Console.CmdTheLine.Util (fileExists)
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), replaceFileName)
import System.IO (hPutStrLn, stderr, writeFile)
import System.Process (ProcessHandle, waitForProcess, createProcess, CreateProcess(..), shell, proc)

waitForSuccess :: String -> ProcessHandle -> IO ()
waitForSuccess cmd p = do
  code <- waitForProcess p
  when (code /= ExitSuccess) $ do
    hPutStrLn stderr $ cmd ++ ": exited with code " ++ show code
    exitFailure

callCommandInDir :: String -> FilePath -> IO ()
callCommandInDir cmd dir = do
  (_, _, _, p) <- createProcess $ (shell cmd) { cwd = Just dir }
  waitForSuccess cmd p

callCommand :: FilePath -> [String] -> IO ()
callCommand path args = do
  (_, _, _, p) <- createProcess $ proc path args
  waitForSuccess path p

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch

-- Read a value from a cache (JSON-encoded file), writing it out if
-- the cached value doesn't exist or is invalid.
fromFileCache :: (FromJSON a, ToJSON a) => FilePath -> a -> IO a
fromFileCache path value = do
  catchAny readCached (\_ -> BL.writeFile path (encode value) >> return value)
 where readCached = do
         d <- BL.readFile path
         case decode d of
           Just x -> return x
           Nothing -> error "Failed to parse JSON."

modulesPath :: IO FilePath
modulesPath = do
  hPath <- hackagePath
  return $ replaceFileName hPath "modules.json"

qualifiedNamesFromModule :: Module -> [(String, String)]
qualifiedNamesFromModule m = [ (mName, name) | Qual (ModuleName mName) (Ident name) <- universeBi m ]

importsFromModule :: Module -> [(String, Maybe String)]
importsFromModule (Module _ _ _ _ _ imports _) = map importName imports
 where importName (ImportDecl _ (ModuleName m) _ _ _ _ Nothing _) = (m, Nothing)
       importName (ImportDecl _ (ModuleName m) _ _ _ _ (Just (ModuleName n)) _) = (m, Just n)

importDecl :: String -> ImportDecl
importDecl m = ImportDecl (SrcLoc "" 0 0) (ModuleName m) True False False Nothing Nothing Nothing

-- packagesFromModules :: ? -> String -> PackageConstraint
packageFromModules modules m =
  Map.findWithDefault (error ("Module \"" ++ m ++ "\" not found in Hackage list.")) (Text.pack m) modules

main = run (term, termInfo)

term = hesh <$> flagStdin <*> optionFile <*> arguments

termInfo = defTI { termName = "hesh", version = "0.3" }

flagStdin :: Term Bool
flagStdin = value . flag $ (optInfo [ "stdin", "s" ]) { optName = "STDIN", optDoc = "If this option is present, or if no arguments remain after option processing, then the script is read from standard input." }

optionFile :: Term String
optionFile = fileExists (value (pos 0 "" posInfo { posName = "FILE" }))

arguments = value (posAny [] posInfo { posName = "ARGS" })

hesh useStdin _ args' = do
  -- First, create a cabal sandbox for the script. We want predictable
  -- results, so we won't share a cabal directory with anything else.
  --
  -- One option would be to create the sandbox in the current
  -- directory, but that would depend on running the script from its
  -- directory, another is to create it in the script's directory, but
  -- we don't know that we have write permissions to that (if it's
  -- even a writable directory). An always writeable option is to use
  -- a temporary directory, however, this has the disadvantage that
  -- it'll could be cleaned up automatically and slow down start time
  -- for later calls of the script. Additionally, how do we name the
  -- directory to avoid collisions? If we just use the script name
  -- there's a high chance of collisions.  If we use a hash of the
  -- script we have to rebuild the directory every time the script
  -- changes (viable for production but not during development).
  --
  -- We can compromise flexibility and safety by providing a command
  -- line parameter for the sandbox directory. For maximum safety,
  -- we'll use a hash of the script contents (this has the added
  -- benefit of ensuring that changes in the script don't break due to
  -- cabal packages already installed), however, the user of the
  -- script can specify a different directory (possibly in a permanent
  -- location).
  --
  -- In order to work in shebang mode and to provide a more familiar
  -- commandline interface, we support taking the input filename as an
  -- argument (rather than just relying on the script being provided
  -- on stdin). If no commandline argument is provided, we assume the
  -- script is on stdin.
  (source', args) <- if useStdin || (args' == [])
                       then (\x -> (x, args')) `fmap` TIO.getContents
                       else (\x -> (x, tail args')) `fmap` TIO.readFile (head args')
  -- Remove any leading shebang line.
  let source = if Text.isPrefixOf "#!" source'
                 then Text.dropWhile (/= '\n') source'
                 else source'
  let md5 = hash $ encodeUtf8 source :: Digest MD5
  dir <- (</> ("hesh-" ++ (Text.unpack $ decodeUtf8 $ digestToHexByteString md5))) `liftM` getTemporaryDirectory
  createDirectoryIfMissing False dir
  -- `cabal sandbox init` accepts a --sandbox argument, but it gets
  -- confused if run in a directory with an existing cabal sandbox (a
  -- likely scenario). To avoid that, we'll use a different working
  -- directory.
  callCommandInDir "cabal sandbox init" dir
  -- Cabal sandboxes do not maintain their own package lists. We have
  -- to rely on whatever the user has updated.
  -- First, get the module -> package+version lookup table.
  p <- modulesPath
  modules <- fromFileCache p =<< modulePackages
  -- Now, parse the script.
  let ast = fromParseResult $ parseModule (Text.unpack source)
      -- Find all qualified module names to add module names to the import list (qualified).
      names = qualifiedNamesFromModule ast
      -- Find any import references.
      imports = importsFromModule ast
      -- Remove aliased modules from the names.
      aliases = catMaybes (map snd imports)
      fqNames = filter (`notElem` aliases) (map fst names)
      -- Insert qualified module usages back into the import list.
      (Module a b c d e importDecls f) = ast
      expandedImports = importDecls ++ map importDecl fqNames
      expandedAst = Module a b c d e expandedImports f
      -- From the imports, build a list of necessary packages.
      packages = map (packageFromModules modules) (map fst imports ++ fqNames)
  writeFile (dir </> "Main.hs") (prettyPrint expandedAst)
  now <- getZonedTime
  -- Cartel expects us to provide the version of the Cartel library
  -- but doesn't export the version for us, so we use 0.
  writeFile (dir </> "script.cabal") $ Cartel.renderString "" now (V.Version [0] []) (cartel (map constrainedPackage packages))
  -- Cabal will complain without a LICENSE file.
  writeFile (dir </> "LICENSE") ""
  callCommandInDir "cabal install" dir
  -- Finally, run the script.
  -- Should we exec() here?
  -- TODO: Set the program name appropriately.
  callCommand (dir </> ".cabal-sandbox/bin/script") args

-- PackageConstraint -> Package
-- Always prefer base, otherwise arbitrarily take the first module.
constrainedPackage ps = Cartel.Package (Text.unpack (packageName package)) Nothing
 where package = case find (\p -> packageName p == (Text.pack "base")) ps of
                   Just p' -> p'
                   Nothing -> head ps

cartel packages = Cartel.empty { Cartel.cProperties = properties
                               , Cartel.cExecutables = [executable] }
 where properties = Cartel.empty
         { Cartel.prName         = "script"
         , Cartel.prVersion      = Cartel.Version [0,1]
         , Cartel.prCabalVersion = (1,18)
         , Cartel.prBuildType    = Cartel.Simple
         , Cartel.prLicense      = Cartel.AllRightsReserved
         , Cartel.prLicenseFile  = "LICENSE"
         , Cartel.prCategory     = "shell"
         }
       executable = Cartel.Executable "script" fields
       fields = [ Cartel.ExeMainIs "Main.hs"
                , Cartel.ExeInfo (Cartel.DefaultLanguage Cartel.Haskell2010)
                , Cartel.ExeInfo (Cartel.BuildDepends ([Cartel.Package "base" Nothing] ++ packages))
                ]

-- We make the simplifying assumption that a module only appears in a
-- contiguous version range.
data PackageConstraint = PackageConstraint { packageName :: Text.Text
                                           , packageMinVersion :: [Int]
                                           , packageMaxVersion :: [Int]
                                           } deriving Generic

instance ToJSON PackageConstraint
instance FromJSON PackageConstraint

modulePackages = foldrWithKey buildConstraints Map.empty `liftM` readHackage
 where buildConstraints name versions constraints = foldrWithKey (buildConstraints' name) constraints versions
       buildConstraints' name version meta constraints = foldr (\m cs -> Map.alter (alterConstraint (Text.pack name) (versionBranch version)) m cs) constraints (map (Text.pack . display) (exposedModules' meta))
       alterConstraint packageName' version constraint =
         case constraint of
           Nothing -> Just [PackageConstraint packageName' version version]
           Just constraints ->
             -- TODO: This could probably be more efficient.
             case find (\c -> packageName c == packageName') constraints of
               -- If the package is already listed, update the constraint.
               Just _ -> Just (map (updateConstraint packageName' version) constraints)
               -- If not, add a new constraint.
               Nothing -> Just $ constraints ++ [PackageConstraint packageName' version version]
       updateConstraint name version constraint = if packageName constraint == name
                                                    then if version < packageMinVersion constraint
                                                           then constraint { packageMinVersion = version }
                                                           else if version < packageMaxVersion constraint
                                                                  then constraint { packageMaxVersion = version }
                                                                  else constraint
                                                    else constraint
       exposedModules' = fromMaybe [] . fmap (exposedModules . condTreeData) . condLibrary
