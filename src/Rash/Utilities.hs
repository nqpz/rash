module Rash.Utilities
  ( xor
  , runProcess
  , Sequence
  , SequenceIO
  , listToSequence
  , sequenceToList
  , sequenceLength
  ) where

import qualified System.Exit as Exit
import qualified System.Process as Proc
import qualified Data.Array.IArray as IA
import qualified Data.Array.IO as IOA


xor :: Bool -> Bool -> Bool
xor x y = x /= y

-- | Run a process with optional standard in, and return the exit code and
-- standard out.
runProcess :: String -> Maybe String -> IO (Int, String)
runProcess cmdAndArgs stdinM = do
  let cp = Proc.shell cmdAndArgs
      stdin = case stdinM of
        Nothing -> ""
        Just s -> s
  (exitCode, stdout, _stderr) <- Proc.readCreateProcessWithExitCode cp stdin
  let i = case exitCode of
        Exit.ExitSuccess -> 0
        Exit.ExitFailure n -> n
  pure (i, stdout)

type Sequence a = IA.Array Int a
type SequenceIO a = IOA.IOArray Int a

listToSequence :: [a] -> Sequence a
listToSequence xs = IA.listArray (0, length xs - 1) xs

sequenceToList :: Sequence a -> [a]
sequenceToList = IA.elems

sequenceLength :: Sequence a -> Int
sequenceLength = (+ 1) . snd . IA.bounds
