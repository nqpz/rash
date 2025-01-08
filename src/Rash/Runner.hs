module Rash.Runner
  ( runFile
  ) where

import qualified Data.Text as T

import qualified Rash.Representation.Internal as RI
import Rash.Interpreter (interpret)
import Rash.IOStateKeeping (retrieveState)

runFile :: RI.IOStateKeeping -> FilePath -> String -> IO ()
runFile ioStateKeeping fname readArgs = do
  (asm, state) <- retrieveState fname readArgs ioStateKeeping
  let context = RI.Context { RI.contextAssembly = asm
                           , RI.contextReadArgs = T.pack readArgs
                           , RI.contextIOStateKeeping = ioStateKeeping
                           }
  interpret context state
