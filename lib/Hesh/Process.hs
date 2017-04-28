{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Hesh.Process ( (|>), (/>), (!>), (&>), (</), (/>>), (!>>), (&>>), (<//), pipeOps
                    , ProcessFailure, cmd, passThrough, (.=)
                    ) where

import Control.Concurrent (forkIO)
import Control.Exception (Exception, bracketOnError)
import Control.Monad (liftM, void)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Typeable (Typeable)
import System.Exit (ExitCode(..))
import System.IO (openFile, IOMode(..), hGetContents, hClose, hPutStr, hPutStrLn, stderr, Handle)
import System.Process (proc, createProcess, CreateProcess(..), ProcessHandle, waitForProcess, StdStream(..), CmdSpec(..), readProcess, terminateProcess)

class Streamable a where
  s_hPutStr :: Handle -> a -> IO ()
  stdoutToStreamable :: (MonadIO m) => m ProcessChain -> m a

instance Streamable String where
  s_hPutStr = hPutStr
  stdoutToStreamable = stdoutToString

instance Streamable Text where
  s_hPutStr = Text.IO.hPutStr
  stdoutToStreamable = stdoutToText

instance Streamable BS.ByteString where
  s_hPutStr = BS.hPutStr
  stdoutToStreamable = stdoutToByteString

type RunningProcess = (String, ProcessHandle)
data ProcessChain = forall a . (Streamable a) => ProcessChain ([RunningProcess], (CreateProcess, Maybe a))

data ProcessFailure = ProcessFailure String Int
  deriving (Typeable)

instance Show ProcessFailure where
  show (ProcessFailure command code) = "Command " ++ command ++ " exited with failure code: " ++ show code

instance Exception ProcessFailure

pipeOps = ["|>", "/>", "!>", "&>", "</", "/>>", "!>>", "&>>", "<//"]

class PipeResult a where
  (|>) :: (MonadIO m) => m ProcessChain -> m ProcessChain -> m a
  (/>) :: (MonadIO m) => m ProcessChain -> FilePath -> m a
  (!>) :: (MonadIO m) => m ProcessChain -> FilePath -> m a
  (&>) :: (MonadIO m) => m ProcessChain -> FilePath -> m a
  (</) :: (MonadIO m) => m ProcessChain -> FilePath -> m a
  (/>>) :: (MonadIO m) => m ProcessChain -> FilePath -> m a
  (!>>) :: (MonadIO m) => m ProcessChain -> FilePath -> m a
  (&>>) :: (MonadIO m) => m ProcessChain -> FilePath -> m a
  (<//) :: (MonadIO m, Streamable s) => m ProcessChain -> s -> m a

instance {-# OVERLAPPING #-} PipeResult ProcessChain where
  (|>) = pipe
  (</) = redirect [Stdin] ReadMode
  (/>) = redirect [Stdout] WriteMode
  (!>) = redirect [Stderr] WriteMode
  (&>) = redirect [Stdout, Stderr] WriteMode
  (/>>) = redirect [Stdout] AppendMode
  (!>>) = redirect [Stderr] AppendMode
  (&>>) = redirect [Stdout, Stderr] AppendMode
  (<//) = readStreamable

instance {-# OVERLAPPING #-} PipeResult () where
  p1 |> p2 = passThrough (p1 |> p2)
  p /> path = passThrough (p /> path)
  p !> path = passThrough (p !> path)
  p &> path = passThrough (p &> path)
  p </ path = passThrough (p </ path)
  p />> path = passThrough (p />> path)
  p !>> path = passThrough (p !>> path)
  p &>> path = passThrough (p &>> path)
  p <// input = passThrough (p <// input)

instance {-# OVERLAPPING #-} (Streamable s) => PipeResult s where
  p1 |> p2 = stdoutToStreamable (p1 |> p2)
  p /> path = stdoutToStreamable (p /> path)
  p !> path = stdoutToStreamable (p !> path)
  p &> path = stdoutToStreamable (p &> path)
  p </ path = stdoutToStreamable (p </ path)
  p />> path = stdoutToStreamable (p />> path)
  p !>> path = stdoutToStreamable (p !>> path)
  p &>> path = stdoutToStreamable (p &>> path)
  p <// input = stdoutToStreamable (p <// input)

-- cmd is like proc but operates on the resulting process depending on
-- its calling context.
class ProcResult a where
  cmd :: (MonadIO m) => FilePath -> [String] -> m a

instance ProcResult ProcessChain where
  cmd path args = return $ ProcessChain ([], (proc path args, Nothing :: Maybe String))

instance ProcResult () where
  cmd path args = passThrough (cmd path args)

instance ProcResult String where
  cmd path args = stdoutToString (cmd path args)

instance ProcResult Text where
  cmd path args = stdoutToText (cmd path args)

instance ProcResult BS.ByteString where
  cmd path args = stdoutToByteString (cmd path args)

waitForSuccess :: [RunningProcess] -> IO ()
waitForSuccess hs = mapM_ waitForSuccess' hs
 where waitForSuccess' (name, handle) = do
         exit <- waitForProcess handle
         case exit of
           ExitSuccess -> return ()
           ExitFailure code -> throwM (ProcessFailure name code)

withProcess :: Streamable s => CreateProcess -> Maybe s -> ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO a) -> IO a
withProcess p s f =
  bracketOnError (createProcess $ maybe p (\ _ -> p { std_in = CreatePipe }) s)
                 (\ (_, _, _, h) -> do terminateProcess h)
                 (\ x@(i, o, e, h) -> case s of
                                        Nothing -> f x
                                        Just s' -> do forkIO $ do
                                                        s_hPutStr (fromJust i) s'
                                                        hClose (fromJust i)
                                                      f (Nothing, o, e, h))

commandName :: CreateProcess -> String
commandName p = let (RawCommand command _) = cmdspec p in command

passThrough :: (MonadIO m) => m ProcessChain -> m ()
passThrough p' = do
  ProcessChain (ps, (p, s)) <- p'
  liftIO (withProcess p s (\ (_, _, _, pHandle) -> waitForSuccess (ps ++ [(commandName p, pHandle)])))

-- TODO: Remove duplication from these functions.
stdoutToString :: (MonadIO m) => m ProcessChain -> m String
stdoutToString p' = do
  ProcessChain (ps, (p, s)) <- p'
  liftIO (withProcess (p { std_out = CreatePipe }) s
                      (\ (_, Just pStdout, _, pHandle) -> do output <- hGetContents pStdout
                                                             waitForSuccess (ps ++ [(commandName p, pHandle)])
  -- Strip any trailing newline. These are almost always added to
  -- programs since shells don't add their own newlines, and it's a
  -- surprise to get these when reading a program's output.
                                                             if not (null output) && last output == '\n'
                                                               then return (init output)
                                                               else return output))

stdoutToText :: (MonadIO m) => m ProcessChain -> m Text
stdoutToText p' = do
  ProcessChain (ps, (p, s)) <- p'
  liftIO (withProcess (p { std_out = CreatePipe }) s
                      (\ (_, Just pStdout, _, pHandle) -> do output <- Text.IO.hGetContents pStdout
                                                             waitForSuccess (ps ++ [(commandName p, pHandle)])
  -- Strip any trailing newline. These are almost always added to
  -- programs since shells don't add their own newlines, and it's a
  -- surprise to get these when reading a program's output.
                                                             if not (Text.null output) && Text.last output == '\n'
                                                               then return (Text.init output)
                                                               else return output))

stdoutToByteString :: (MonadIO m) => m ProcessChain -> m BS.ByteString
stdoutToByteString p' = do
  ProcessChain (ps, (p, s)) <- p'
  liftIO (withProcess (p { std_out = CreatePipe }) s
                      (\ (_, Just pStdout, _, pHandle) -> do output <- BS.hGetContents pStdout
                                                             waitForSuccess (ps ++ [(commandName p, pHandle)])
  -- Strip any trailing newline. These are almost always added to
  -- programs since shells don't add their own newlines, and it's a
  -- surprise to get these when reading a program's output.
                                                             if not (BS.null output) && BS.last output == '\n'
                                                               then return (BS.init output)
                                                               else return output))

pipe :: (MonadIO m) => m ProcessChain -> m ProcessChain -> m ProcessChain
pipe p1' p2' = do
  ProcessChain (ps1, (p1, s1)) <- p1'
  ProcessChain (ps2, (p2, _)) <- p2'
  liftIO (withProcess (p1 { std_out = CreatePipe }) s1
                      (\ (_, Just p1Stdout, _, p1Handle) -> return $ ProcessChain (ps1 ++ [(commandName p1, p1Handle)] ++ ps2, (p2 { std_in = UseHandle p1Stdout }, Nothing :: Maybe String))))

readStreamable :: (MonadIO m, Streamable s) => m ProcessChain -> s -> m ProcessChain
readStreamable p' s = do
  ProcessChain (ps, (p, _)) <- p'
  return $ ProcessChain (ps, (p, Just s))

data StdHandle = Stdin | Stdout | Stderr deriving (Eq)

redirect :: (MonadIO m) => [StdHandle] -> IOMode -> m ProcessChain -> FilePath -> m ProcessChain
redirect handles mode p' path = do
  ProcessChain (ps, (p, s)) <- p'
  f <- liftIO (openFile path mode)
  return (case handles of
             [Stdin] -> ProcessChain (ps, (p { std_in = UseHandle f }, s))
             [Stdout] -> ProcessChain (ps, (p { std_out = UseHandle f }, s))
             [Stderr] -> ProcessChain (ps, (p { std_err = UseHandle f }, s))
             [Stdout, Stderr] -> ProcessChain (ps, (p { std_out = UseHandle f, std_err = UseHandle f }, s)))
  -- TODO: Close handle(s).

-- I'm not sure that I want this to stick around, so I'm not
-- documenting it. If it's common enough, it's worth keeping. If not,
-- it might just be confusing.
(.=) :: (MonadIO m) => m String -> m String -> m Bool
(.=) lhs' rhs' = do
  lhs <- lhs'
  rhs <- rhs'
  return (lhs == rhs)
