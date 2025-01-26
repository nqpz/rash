module InterpreterTests
  ( tests
  ) where

import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)

import Paths_rash (getDataFileName)

import qualified Rash.Representation.Internal as RI
import qualified Rash.Interpreter as Interp

import Test.Tasty
import Test.Tasty.HUnit

makeTester :: FilePath -> IO ([String] -> IO (),
                              IO [String],
                              [String] -> IO ())
makeTester path = do
  fname <- getDataFileName path
  asmMRef <- newIORef Nothing
  stateMRef <- newIORef Nothing
  linesRef <- newIORef []
  let ioStateKeeping = RI.InMemory asmMRef stateMRef (\line -> modifyIORef linesRef (line :))
      interpret args = do
        writeIORef linesRef []
        Interp.interpret fname args ioStateKeeping
      getActual = do
        actualReversed <- readIORef linesRef
        pure $ reverse actualReversed
      check expected = do
        actual <- getActual
        when (actual /= expected) $ fail ("Expected " ++ show expected ++ " but got " ++ show actual)
  pure (interpret, getActual, check)

addTest :: TestTree
addTest = testCase "add" $ do
  (interpret, _, check) <- makeTester "examples/add.rash"

  interpret []
  check ["Please enter the first number.\n"]
  interpret ["4"]
  check ["Please enter the second number.\n"]
  interpret ["91"]
  check ["4 + 91 = 95\n"]

tests :: TestTree
tests = testGroup "Interpreter tests" [addTest]
