module Main (main) where

import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified Data.List as L

import Rash.StateDirGetter (getOrCreateStateDir)
import qualified Rash.Representation.Internal as RI
import Rash.Runner (runFile)

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    (fname : readArgs) -> do
      stateDir <- getOrCreateStateDir
      runFile RI.WriteAndReadFiles stateDir fname (L.intercalate " " readArgs)
    [] -> Exit.exitFailure
