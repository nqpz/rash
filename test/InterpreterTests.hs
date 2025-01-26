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

guessNumberTest :: TestTree
guessNumberTest = testCase "guess-number" $ do
  (interpret, getActual, check) <- makeTester "examples/guess-number.rash"
  let guess :: Int -> Int -> IO ()
      guess start end = do
        let n = start + (end - start) `div` 2
        interpret [show n]
        actual <- getActual
        -- Ignore the empty outputs of running 'test'.
        let response = case reverse actual of
              echo_output : _ -> echo_output
              _ -> error ("Unexpected output: " ++ show actual)
        case response of
          "Your guess was too low, try again.\n" ->
            guess (n + 1) end
          "Your guess was too high, try again.\n" ->
            guess start (n - 1)
          "Congratulations, you guessed the number!\n" ->
            pure ()
          _ -> error ("Unexpected response:" ++ show response)

  interpret []
  check ["Guess the number!\n"]
  guess 1 100

tests :: TestTree
tests = testGroup "Interpreter tests"
  [ addTest
  , guessNumberTest
  ]
