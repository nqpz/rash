module InterpreterTests
  ( test
  ) where

import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)

import Paths_rash (getDataFileName)

import qualified Rash.Representation.Internal as RI
import qualified Rash.Interpreter as Interp

test :: IO ()
test = do
  fname <- getDataFileName "test/programs/add.rash"
  asmMRef <- newIORef Nothing
  stateMRef <- newIORef Nothing
  linesRef <- newIORef []
  let ioStateKeeping = RI.InMemory asmMRef stateMRef (\line -> modifyIORef linesRef (line :))
      interpret args = Interp.interpret fname args ioStateKeeping
      check expected = do
        actualReversed <- readIORef linesRef
        let actual = reverse actualReversed
        when (actual /= expected)
          $ fail ("Expected " ++ show expected ++ " but got " ++ show actual)
        writeIORef linesRef []

  interpret []
  check ["Please enter the first number.\n"]
  interpret ["4"]
  check ["Please enter the second number.\n"]
  interpret ["91"]
  check ["4 + 91 = 95\n"]
