module Rash.StateDirGetter (getOrCreateStateDir) where

import Control.Exception (try)
import qualified System.Directory as Dir
import qualified System.Environment as Env
import System.FilePath ((</>))

firstJust :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
firstJust mma mmb = do
  ma <- mma
  case ma of
    Just a -> pure $ Just a
    Nothing -> mmb

lookupEnv :: String -> IO (Maybe String)
lookupEnv envVar = do
  lookupResult <- Env.lookupEnv envVar
  pure $ case lookupResult of
    Nothing -> Nothing
    Just "" -> Nothing
    Just s -> Just s

rashEnvVar :: String
rashEnvVar = "RASH_STATE_DIR"

xdgEnvVar :: String
xdgEnvVar = "XDG_HOME_STATE"

homeEnvVar :: String
homeEnvVar = "HOME"

getStateDir :: IO FilePath
getStateDir = do
  stateDirResult <-
    firstJust
    (lookupEnv rashEnvVar)
    (firstJust
     (((</> "rash") <$>) <$> lookupEnv xdgEnvVar)
     (((</> ".local" </> "state" </> "rash") <$>) <$> lookupEnv homeEnvVar))
  case stateDirResult of
    Just stateDir ->
      pure stateDir
    Nothing ->
      fail ("Neither "
            ++ rashEnvVar
            ++ ", "
            ++ xdgEnvVar
            ++ ", or"
            ++ homeEnvVar
            ++ " are set. Cannot determine state directory for rash.")

getOrCreateStateDir :: IO FilePath
getOrCreateStateDir = do
  stateDir <- getStateDir
  createResult <-
    try (Dir.createDirectoryIfMissing True stateDir) :: IO (Either IOError ())
  case createResult of
    Left exception ->
      fail ("Could not create the directory "
            ++ stateDir
            ++ ": "
            ++ show exception)
    Right () ->
      pure stateDir
