module Main (main) where

import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.Directory as Dir
import qualified System.FilePath as Path
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
      paths <- rashPaths stateDir fname
      runFile (RI.WriteAndReadFiles paths) fname (L.intercalate " " readArgs)
    [] -> Exit.exitFailure

rashPaths :: FilePath -> FilePath -> IO RI.RashPaths
rashPaths stateDir fname = do
  fnameCanon <- Dir.canonicalizePath fname
  let fnameSave = stateDir ++ fnameCanon
      dirSave = fst $ Path.splitFileName fnameSave
      asmSave = fnameSave ++ ".asm"
      stateSave = fnameSave ++ ".state"
  pure RI.RashPaths { RI.pathDir = dirSave
                    , RI.pathASM = asmSave
                    , RI.pathState = stateSave
                    }
