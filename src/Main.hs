module Main (main) where

import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified Data.List as L

import Rash.Runner (runFile)

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    (fname : readArgs) -> do
      stateDir <- Env.getEnv "RASH_STATE_DIR"
      runFile stateDir fname (L.intercalate " " readArgs)
    [] -> Exit.exitFailure
