module Rash.StateDirGetter (getStateDir) where

import Control.Exception (try)
import qualified System.Directory as Dir
import qualified System.Environment as Env

lookupEnv :: String -> IO (Maybe String)
lookupEnv envVar = do
  lookupResult <- Env.lookupEnv envVar
  pure $ case lookupResult of
    Nothing -> Nothing
    Just "" -> Nothing
    Just s -> Just s

stateDirEnvVar :: String
stateDirEnvVar = "RASH_STATE_DIR"

stateDirEnvVarEmptyError :: IO a
stateDirEnvVarEmptyError =
  fail ("Could not run. Please set the "
        ++ stateDirEnvVar
        ++ " environment variable to a directory.")

stateDirEnvVarCouldNotCreateDir :: FilePath -> IOError -> IO a
stateDirEnvVarCouldNotCreateDir stateDir exception =
  fail ("Could not create the directory "
        ++ stateDir
        ++ " from the environment variable "
        ++ stateDirEnvVar
        ++ ": "
        ++ show exception)

getStateDir :: IO FilePath
getStateDir = do
  lookupResult <- lookupEnv stateDirEnvVar
  stateDir <- case lookupResult of
    Just s -> pure s
    Nothing -> stateDirEnvVarEmptyError
  createResult <- try (Dir.createDirectoryIfMissing True stateDir) :: IO (Either IOError ())
  case createResult of
    Left exception -> stateDirEnvVarCouldNotCreateDir stateDir exception
    Right () -> pure stateDir
