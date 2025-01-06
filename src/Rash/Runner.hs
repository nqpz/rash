module Rash.Runner
  ( runFile
  ) where

import qualified System.Directory as Dir
import qualified System.FilePath as Path
import qualified Data.Text as T

import qualified Rash.Representation.Internal as RI
import Rash.Interpreter (interpret)
import Rash.IOStateKeeping (retrieveState)

runFile :: RI.IOStateKeeping -> FilePath -> FilePath -> String -> IO ()
runFile ioStateKeeping stateDir fname readArgs = do
  paths <- rashPaths stateDir fname
  (asm, state) <- retrieveState paths fname readArgs ioStateKeeping
  let context = RI.Context { RI.contextAssembly = asm
                           , RI.contextReadArgs = T.pack readArgs
                           , RI.contextPaths = paths
                           , RI.contextIOStateKeeping = ioStateKeeping
                           }
  interpret context state

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
