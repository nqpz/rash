module Main (main) where

import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.Directory as Dir
import qualified System.FilePath as Path
import qualified Data.List as L
import qualified Data.Text as T

import Rash.StateDirGetter (getOrCreateStateDir)
import qualified Rash.Representation.Internal as RI
import Rash.Interpreter (interpret)
import Rash.IOStateKeeping (retrieveState)

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    (fname : readArgs) -> do
      stateDir <- getOrCreateStateDir
      paths <- rashPaths stateDir fname
      let ioStateKeeping = RI.WriteAndReadFiles paths
          readArgs' = L.intercalate " " readArgs
      (asm, state) <- retrieveState fname readArgs' ioStateKeeping
      let context = RI.Context { RI.contextAssembly = asm
                               , RI.contextReadArgs = T.pack readArgs'
                               , RI.contextIOStateKeeping = ioStateKeeping
                               }
      interpret context state
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
