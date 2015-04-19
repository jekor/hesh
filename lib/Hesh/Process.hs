{-# LANGUAGE FlexibleInstances #-}

module Hesh.Process ( (|>), (/>), (!>), (&>), (</), pipeOps
                    , cmd, passThrough, (.=)
                    ) where

import Control.Exception (finally)
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isSpace)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import System.Exit (ExitCode(..))
import System.IO (openFile, IOMode(..), hGetContents, hClose, hPutStrLn, stderr)
import System.Process (proc, createProcess, CreateProcess(..), ProcessHandle, waitForProcess, StdStream(..), CmdSpec(..), readProcess)

pipeOps = ["|>", "/>", "!>", "&>", "</"]

class PipeResult a where
  (|>) :: (MonadIO m) => m CreateProcess -> m CreateProcess -> m a
  (/>) :: (MonadIO m) => m CreateProcess -> FilePath -> m a
  (!>) :: (MonadIO m) => m CreateProcess -> FilePath -> m a
  (&>) :: (MonadIO m) => m CreateProcess -> FilePath -> m a
  (</) :: (MonadIO m) => m CreateProcess -> FilePath -> m a

instance PipeResult CreateProcess where
  (|>) = pipe
  (</) = redirect [Stdin]
  (/>) = redirect [Stdout]
  (!>) = redirect [Stderr]
  (&>) = redirect [Stdout, Stderr]

instance PipeResult () where
  p1 |> p2 = passThrough (p1 |> p2)
  p /> path = passThrough (p /> path)
  p !> path = passThrough (p !> path)
  p &> path = passThrough (p &> path)
  p </ path = passThrough (p </ path)

-- cmd is like proc but operates on the resulting process depending on
-- its calling context.
class ProcResult a where
  cmd :: (MonadIO m) => FilePath -> [String] -> m a

instance ProcResult CreateProcess where
  cmd path args = return (proc path args)

instance ProcResult () where
  cmd path args = passThrough (cmd path args)

instance ProcResult String where
  cmd path args = stdoutToString (cmd path args)

-- TODO: Use something like withCreateProcess to handle exceptions.
waitForSuccess :: CreateProcess -> ProcessHandle -> IO ()
waitForSuccess p h = do
  exit <- waitForProcess h
  case exit of
    ExitSuccess -> return ()
    ExitFailure code -> let (RawCommand command _) = cmdspec p in
                          error ("Command " ++ command ++ " exited with failure code: " ++ show code)  

passThrough :: (MonadIO m) => m CreateProcess -> m ()
passThrough p' = do
  p <- p'
  (_, _, _, pHandle) <- liftIO (createProcess p)
  liftIO (waitForSuccess p pHandle)

stdoutToString :: (MonadIO m) => m CreateProcess -> m String
stdoutToString p' = do
  p <- p'
  (_, Just pStdout, _, pHandle) <- liftIO (createProcess p { std_out = CreatePipe })
  output <- liftIO (hGetContents pStdout)
  liftIO (waitForSuccess p pHandle)
  -- Strip any trailing newline. These are almost always added to
  -- programs since shells don't add their own newlines, and it's a
  -- surprise to get these when reading a program's output.
  if last output == '\n'
    then return (init output)
    else return output

pipe :: (MonadIO m) => m CreateProcess -> m CreateProcess -> m CreateProcess
pipe p1' p2' = do
  p1 <- p1'; p2 <- p2'
  (_, Just p1Stdout, _, _) <- liftIO (createProcess p1 { std_out = CreatePipe })
  -- TODO: Provide a way to fail on p1 failure (pipefail).
  return (p2 { std_in = UseHandle p1Stdout })

data StdHandle = Stdin | Stdout | Stderr deriving (Eq)

redirect :: (MonadIO m) => [StdHandle] -> m CreateProcess -> FilePath -> m CreateProcess
redirect handles p' path = do
  p <- p'
  f <- liftIO (openFile path (if handles == [Stdin] then ReadMode else WriteMode))
  return (case handles of
             [Stdin] -> p { std_in = UseHandle f }
             [Stdout] -> p { std_out = UseHandle f }
             [Stderr] -> p { std_err = UseHandle f }
             [Stdout, Stderr] -> p { std_out = UseHandle f, std_err = UseHandle f })
  -- TODO: Close handle(s).

-- I'm not sure that I want this to stick around, so I'm not
-- documenting it. If it's common enough, it's worth keeping. If not,
-- it might just be confusing.
(.=) :: (MonadIO m) => m String -> m String -> m Bool
(.=) lhs' rhs' = do
  lhs <- lhs'
  rhs <- rhs'
  return (lhs == rhs)
