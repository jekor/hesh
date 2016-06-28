{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Hesh.Process ( (|>), (/>), (!>), (&>), (</), (/>>), (!>>), (&>>), pipeOps
                    , ProcessFailure, cmd, passThrough, (.=)
                    ) where

import Control.Exception (Exception, bracketOnError, throwIO)
import Control.Monad (liftM, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isSpace)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Typeable (Typeable)
import System.Exit (ExitCode(..))
import System.IO (openFile, IOMode(..), hGetContents, hClose, hPutStrLn, stderr, Handle)
import System.Process (proc, createProcess, CreateProcess(..), ProcessHandle, waitForProcess, StdStream(..), CmdSpec(..), readProcess, terminateProcess)

type RunningProcess = (String, ProcessHandle)
type ProcessChain = ([RunningProcess], CreateProcess)

data ProcessFailure = ProcessFailure String Int
  deriving (Typeable)

instance Show ProcessFailure where
  show (ProcessFailure command code) = "Command " ++ command ++ " exited with failure code: " ++ show code

instance Exception ProcessFailure

pipeOps = ["|>", "/>", "!>", "&>", "</", "/>>", "!>>", "&>>"]

class PipeResult a where
  (|>) :: (MonadIO m) => m ProcessChain -> m ProcessChain -> m a
  (/>) :: (MonadIO m) => m ProcessChain -> FilePath -> m a
  (!>) :: (MonadIO m) => m ProcessChain -> FilePath -> m a
  (&>) :: (MonadIO m) => m ProcessChain -> FilePath -> m a
  (</) :: (MonadIO m) => m ProcessChain -> FilePath -> m a
  (/>>) :: (MonadIO m) => m ProcessChain -> FilePath -> m a
  (!>>) :: (MonadIO m) => m ProcessChain -> FilePath -> m a
  (&>>) :: (MonadIO m) => m ProcessChain -> FilePath -> m a

instance PipeResult ProcessChain where
  (|>) = pipe
  (</) = redirect [Stdin] ReadMode
  (/>) = redirect [Stdout] WriteMode
  (!>) = redirect [Stderr] WriteMode
  (&>) = redirect [Stdout, Stderr] WriteMode
  (/>>) = redirect [Stdout] AppendMode
  (!>>) = redirect [Stderr] AppendMode
  (&>>) = redirect [Stdout, Stderr] AppendMode

instance PipeResult () where
  p1 |> p2 = passThrough (p1 |> p2)
  p /> path = passThrough (p /> path)
  p !> path = passThrough (p !> path)
  p &> path = passThrough (p &> path)
  p </ path = passThrough (p </ path)
  p />> path = passThrough (p />> path)
  p !>> path = passThrough (p !>> path)
  p &>> path = passThrough (p &>> path)

instance PipeResult String where
  p1 |> p2 = stdoutToString (p1 |> p2)
  p /> path = stdoutToString (p /> path)
  p !> path = stdoutToString (p !> path)
  p &> path = stdoutToString (p &> path)
  p </ path = stdoutToString (p </ path)
  p />> path = stdoutToString (p />> path)  
  p !>> path = stdoutToString (p !>> path)  
  p &>> path = stdoutToString (p &>> path)  

-- cmd is like proc but operates on the resulting process depending on
-- its calling context.
class ProcResult a where
  cmd :: (MonadIO m) => FilePath -> [String] -> m a

instance ProcResult ProcessChain where
  cmd path args = return ([], proc path args)

instance ProcResult () where
  cmd path args = passThrough (cmd path args)

instance ProcResult String where
  cmd path args = stdoutToString (cmd path args)

waitForSuccess :: [RunningProcess] -> IO ()
waitForSuccess hs = mapM_ waitForSuccess' hs
 where waitForSuccess' (name, handle) = do
         exit <- waitForProcess handle
         case exit of
           ExitSuccess -> return ()
           ExitFailure code -> throwIO (ProcessFailure name code)

withProcess :: CreateProcess -> ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO a) -> IO a
withProcess p f =
  bracketOnError (createProcess p)
                 (\ (_, _, _, h) -> do terminateProcess h)
                 f

commandName :: CreateProcess -> String
commandName p = let (RawCommand command _) = cmdspec p in command

passThrough :: (MonadIO m) => m ProcessChain -> m ()
passThrough p' = do
  (ps, p) <- p'
  liftIO (withProcess p (\ (_, _, _, pHandle) -> waitForSuccess (ps ++ [(commandName p, pHandle)])))

stdoutToString :: (MonadIO m) => m ProcessChain -> m String
stdoutToString p' = do
  (ps, p) <- p'
  liftIO (withProcess (p { std_out = CreatePipe })
                      (\ (_, Just pStdout, _, pHandle) -> do output <- hGetContents pStdout
                                                             waitForSuccess (ps ++ [(commandName p, pHandle)])
  -- Strip any trailing newline. These are almost always added to
  -- programs since shells don't add their own newlines, and it's a
  -- surprise to get these when reading a program's output.
                                                             if not (null output) && last output == '\n'
                                                               then return (init output)
                                                               else return output))

pipe :: (MonadIO m) => m ProcessChain -> m ProcessChain -> m ProcessChain
pipe p1' p2' = do
  (ps1, p1) <- p1'
  (ps2, p2) <- p2'
  liftIO (withProcess (p1 { std_out = CreatePipe })
                      (\ (_, Just p1Stdout, _, p1Handle) -> return (ps1 ++ [(commandName p1, p1Handle)] ++ ps2, p2 { std_in = UseHandle p1Stdout })))

data StdHandle = Stdin | Stdout | Stderr deriving (Eq)

redirect :: (MonadIO m) => [StdHandle] -> IOMode -> m ProcessChain -> FilePath -> m ProcessChain
redirect handles mode p' path = do
  (ps, p) <- p'
  f <- liftIO (openFile path mode)
  return (case handles of
             [Stdin] -> (ps, p { std_in = UseHandle f })
             [Stdout] -> (ps, p { std_out = UseHandle f })
             [Stderr] -> (ps, p { std_err = UseHandle f })
             [Stdout, Stderr] -> (ps, p { std_out = UseHandle f, std_err = UseHandle f }))
  -- TODO: Close handle(s).

-- I'm not sure that I want this to stick around, so I'm not
-- documenting it. If it's common enough, it's worth keeping. If not,
-- it might just be confusing.
(.=) :: (MonadIO m) => m String -> m String -> m Bool
(.=) lhs' rhs' = do
  lhs <- lhs'
  rhs <- rhs'
  return (lhs == rhs)
