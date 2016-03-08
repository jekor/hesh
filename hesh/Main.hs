{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Cartel
import qualified Cartel.Ast
import qualified Cartel.Render
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mapM_, liftM, when)
import Control.Exception (catch, throw, SomeException)
import Crypto.Hash (hash, digestToHexByteString, Digest, MD5)
import Data.Aeson (Value(..), ToJSON(..), FromJSON(..), encode, decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Generics.Uniplate.Data (universeBi, transformBi)
import Data.List (intercalate, find, filter, nub, any, takeWhile, isPrefixOf, unionBy)
import qualified Data.Map.Strict as Map
import Data.Map.Lazy (foldrWithKey)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid (mempty)
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
import Language.Haskell.Exts (parseFileContentsWithMode, ParseMode(..), defaultParseMode, Extension(..), KnownExtension(..), ParseResult(..), fromParseResult)
import Language.Haskell.Exts.Syntax (Module(..), ModuleName(..), ModulePragma(..), ImportDecl(..), QName(..), Name(..), Exp(..), Stmt(..), Type(..), SrcLoc(..), QOp(..))
import Language.Haskell.Exts.Pretty (prettyPrintWithMode, defaultMode, PPHsMode(linePragmas))
import System.Console.CmdTheLine (Term, TermInfo(..), OptInfo(..), PosInfo(..), flag, value, pos, posAny, optInfo, posInfo, defTI)
import System.Console.CmdTheLine.Term (run)
import System.Console.CmdTheLine.Util (fileExists)
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), (<.>), replaceFileName, takeBaseName)
import System.IO (hPutStrLn, stderr, writeFile)
import System.Process (ProcessHandle, waitForProcess, createProcess, CreateProcess(..), shell, proc, StdStream(..))

import Hesh.Process (pipeOps)
import Hesh.Shell (desugar)

waitForSuccess :: String -> ProcessHandle -> IO ()
waitForSuccess cmd p = do
  code <- waitForProcess p
  when (code /= ExitSuccess) $ do
    hPutStrLn stderr $ cmd ++ ": exited with code " ++ show code
    exitFailure

callCommandInDir :: String -> FilePath -> IO ()
callCommandInDir cmd dir = do
  -- GHC is noisy on stdout. It should go to stderr instead.
  -- TODO: Move this into a more appropriate place. It just
  -- happens to work here.
  (_, _, _, p) <- createProcess (shell cmd) { cwd = Just dir, std_out = UseHandle stderr }
  waitForSuccess cmd p

callCommand :: FilePath -> [String] -> IO ()
callCommand path args = do
  (_, _, _, p) <- createProcess (proc path args)
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

-- This is a helper until Haskell has better defaulting (something like
-- https://ghc.haskell.org/trac/haskell-prime/wiki/Defaulting).
-- It saves adding "IO ()" type declarations in a number of common contexts.
defaultToUnit :: Module -> Module
defaultToUnit = transformBi defaultExpToUnit
 where defaultExpToUnit :: Exp -> Exp
       defaultExpToUnit (Do stmts) = Do (map defaultStmtToUnit stmts)
       defaultExpToUnit exp = exp
       defaultStmtToUnit (Qualifier exp) = Qualifier (defaultQualifiedExpToUnit exp)
       defaultStmtToUnit stmt = stmt
       defaultQualifiedExpToUnit exp = if canDefaultToUnit exp then defaultToUnit exp else exp
       -- do [sh| ... |] => do [sh| ... |] :: IO ()
       canDefaultToUnit (QuasiQuote f _) = f `elem` ["sh", "Hesh.sh"]
       -- do cmd "..." [...] => do cmd "..." [...] :: IO ()
       canDefaultToUnit (App (App (Var f) _) _) = f `elem` functionNames "cmd"
       -- do ... /> "..." => ... /> "..." :: IO ()
       canDefaultToUnit (InfixApp exp1 (QVarOp op) exp2) = op `elem` (concatMap operatorNames pipeOps)
       canDefaultToUnit _ = False
       defaultToUnit exp = ExpTypeSig (SrcLoc "<generated>" 0 0) exp (TyVar (Ident "IO ()"))
       functionNames name = [ UnQual (Ident name)
                            , Qual (ModuleName "Hesh") (Ident name) ]
       operatorNames name = [ UnQual (Symbol name)
                            , UnQual (Ident ("(" ++ name ++ ")"))
                            , Qual (ModuleName "Hesh") (Ident ("(" ++ name ++ ")")) ]

importsFromModule :: Module -> [(String, Maybe String, Maybe String)]
importsFromModule (Module _ _ _ _ _ imports _) = map importName imports
 where importName (ImportDecl _ (ModuleName m) _ _ _ pkg Nothing _) = (m, Nothing, pkg)
       importName (ImportDecl _ (ModuleName m) _ _ _ pkg (Just (ModuleName n)) _) = (m, Just n, pkg)

importDeclQualified :: String -> ImportDecl
importDeclQualified m = ImportDecl (SrcLoc "<generated>" 0 0) (ModuleName m) True False False Nothing Nothing Nothing

importDeclUnqualified :: String -> ImportDecl
importDeclUnqualified m = ImportDecl (SrcLoc "<generated>" 0 0) (ModuleName m) False False False Nothing Nothing Nothing

packageFromModules modules (m, _, Just pkg)
  | pkg == "hesh" = Cartel.package "hesh" Cartel.anyVersion -- [1, 3, 0] [1, 3, 0]
  | otherwise = Cartel.package pkg Cartel.anyVersion
packageFromModules modules (m, _, Nothing)
  | m == "Hesh" || isPrefixOf "Hesh." m = Cartel.package "hesh" Cartel.anyVersion
  | otherwise = constrainedPackage (Map.findWithDefault (error ("Module \"" ++ m ++ "\" not found in Hackage list.")) (Text.pack m) modules)

main = run (term, termInfo)

term = hesh <$> flagStdin <*> flagNoSugar <*> optionFile <*> arguments

termInfo = defTI { termName = "hesh", version = "1.3.0" }

flagStdin :: Term Bool
flagStdin = value . flag $ (optInfo [ "stdin", "s" ]) { optName = "STDIN", optDoc = "If this option is present, or if no arguments remain after option processing, then the script is read from standard input." }

flagNoSugar :: Term Bool
flagNoSugar = value . flag $ (optInfo [ "no-sugar", "n" ]) { optName = "NOSUGAR", optDoc = "Don't expand syntax shortcuts." }

optionFile :: Term String
optionFile = value (pos 0 "" posInfo { posName = "FILE" })

arguments = value (posAny [] posInfo { posName = "ARGS" })

hesh useStdin noSugar _ args' = do
  -- In order to work in shebang mode and to provide a more familiar
  -- commandline interface, we support taking the input filename as an
  -- argument (rather than just relying on the script being provided
  -- on stdin). If no commandline argument is provided, we assume the
  -- script is on stdin.
  let scriptFile = if useStdin || (args' == []) then "<stdin>" else head args'
      scriptName = if useStdin || (args' == []) then "script" else takeBaseName (head args')
  (source'', args) <- if useStdin || (args' == [])
                        then (\x -> (x, args')) `fmap` TIO.getContents
                        else (\x -> (x, tail args')) `fmap` TIO.readFile (head args')
  -- Remove any leading shebang line.
  let source' = if Text.isPrefixOf "#!" source''
                 then Text.dropWhile (/= '\n') source''
                 else source''
      source = if noSugar
                 then source'
                 else Text.pack (desugar (Text.unpack source'))
      md5 = hash $ encodeUtf8 source :: Digest MD5
  --
  -- First, create a directory for the script. One option would be to
  -- create the directory in the current directory, but that would
  -- depend on running the script from its directory, another is to
  -- create it in the script's directory, but we don't know that we
  -- have write permissions to that (if it's even a writable
  -- directory). An always writeable option is to use a temporary
  -- directory, however, this has the disadvantage that it'll could be
  -- cleaned up automatically and slow down start time for later calls
  -- of the script. Additionally, how do we name the directory to
  -- avoid collisions? If we just use the script name there's a high
  -- chance of collisions. If we use a hash of the script we have to
  -- rebuild the directory every time the script changes (viable for
  -- production but not during development).
  --
  -- We could compromise flexibility and safety by providing a command
  -- line parameter for the directory. For maximum safety, we'll use a
  -- hash of the script contents for now.
  --
  dir <- (</> ("hesh-" ++ (Text.unpack $ decodeUtf8 $ digestToHexByteString md5))) `liftM` getTemporaryDirectory
  hPutStrLn stderr ("Building in " ++ dir)
  createDirectoryIfMissing False dir
  -- First, get the module -> package+version lookup table.
  p <- modulesPath
  modules <- fromFileCache p =<< modulePackages
  -- Now, parse the script.
  let ast = defaultToUnit (parseScript scriptFile noSugar source)
      -- Find all qualified module names to add module names to the import list (qualified).
      names = qualifiedNamesFromModule ast
      -- Find any import references.
      imports = importsFromModule ast
      -- Remove aliased modules from the names.
      aliases = catMaybes (map (\ (_, y, _) -> y) imports)
      fqNames = filter (`notElem` aliases) (map fst names)
      -- Insert qualified module usages back into the import list.
      (Module a b pragmas d e importDecls g) = ast
      expandedImports = importDecls ++ map importDeclQualified fqNames ++ if noSugar then [] else [importDeclUnqualified "Hesh"]
      expandedPragmas = if noSugar then pragmas else pragmas ++ sugarPragmas
      expandedAst = Module a b expandedPragmas d e expandedImports g
      -- From the imports, build a list of necessary packages.
      -- First, remove fully qualified names that were previously
      -- imported explicitly. This is so that we don't override the
      -- package that might have been selected for that module
      -- manually in the import statement.
      packages = map (packageFromModules modules) (unionBy (\ (x, _, _) (x', _, _) -> x == x') imports (map fqNameModule fqNames) ++ if noSugar then [] else [("Hesh", Nothing, Just "hesh")])
      -- packages = map (packageFromModules modules) (map fst imports ++ fqNames ++ if noSugar then [] else ["Hesh"])
  writeFile (dir </> "Main.hs") (prettyPrintWithMode (defaultMode { linePragmas = True }) expandedAst)
  now <- getZonedTime
  -- Cartel expects us to provide the version of the Cartel library
  -- but doesn't export the version for us, so we use 0.
  writeFile (dir </> scriptName <.> "cabal") (Cartel.Render.renderNoIndent (cartel packages scriptName))
  -- Cabal will complain without a LICENSE file.
  writeFile (dir </> "LICENSE") ""
  callCommandInDir "cabal install --only-dependencies" dir
  callCommandInDir "cabal build" dir
  -- Finally, run the script.
  -- Should we exec() here?
  -- TODO: Set the program name appropriately.
  callCommand (dir </> "dist/build" </> scriptName </> scriptName) args
 where fqNameModule name = (name, Nothing, Nothing)
       sugarPragmas = [LanguagePragma (SrcLoc "<generated>" 0 0) [Ident "TemplateHaskell", Ident "QuasiQuotes", Ident "PackageImports"]]

cartel packages name = mempty { Cartel.Ast.properties = properties
                              , Cartel.Ast.sections = [executable] }
 where properties = mempty
         { Cartel.name         = name
         , Cartel.version      = [0,1]
         , Cartel.cabalVersion = Just (fromIntegral 1, fromIntegral 18)
         , Cartel.buildType    = Just Cartel.simple
         , Cartel.license      = Just Cartel.allRightsReserved
         , Cartel.licenseFile  = "LICENSE"
         , Cartel.category     = "shell"
         }
       executable = Cartel.executable name fields
       fields = [ Cartel.Ast.ExeMainIs "Main.hs"
                , Cartel.Ast.ExeInfo (Cartel.Ast.DefaultLanguage Cartel.Ast.Haskell2010)
                , Cartel.Ast.ExeInfo (Cartel.Ast.BuildDepends ([Cartel.package "base" Cartel.anyVersion] ++ packages))
                ]

-- We make the simplifying assumption that a module only appears in a
-- contiguous version range.
data PackageConstraint = PackageConstraint { packageName :: Text.Text
                                           , packageMinVersion :: [Int]
                                           , packageMaxVersion :: [Int]
                                           } deriving Generic

instance ToJSON PackageConstraint
instance FromJSON PackageConstraint

-- PackageConstraint -> Package
-- Always prefer base, otherwise arbitrarily take the first module.
constrainedPackage ps = Cartel.package (Text.unpack (packageName package)) Cartel.anyVersion
 where package = case find (\p -> packageName p == (Text.pack "base")) ps of
                   Just p' -> p'
                   Nothing -> head ps

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

parseScript :: String -> Bool -> Text.Text -> Module
parseScript filename noSugar source =
  case parseFileContentsWithMode
       (defaultParseMode {parseFilename = filename, extensions = exts})
       (Text.unpack source) of
    ParseOk m -> m
    r@(ParseFailed _ _) -> fromParseResult r
 where exts = if noSugar then [] else [EnableExtension TemplateHaskell, EnableExtension QuasiQuotes, EnableExtension PackageImports]
