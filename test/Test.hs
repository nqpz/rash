module Main (main) where

import qualified InterpreterTests

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ InterpreterTests.tests
  ]
