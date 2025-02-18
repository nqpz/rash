module Rash.Representation.Internal
  ( RashPaths(..)
  , ID
  , Part(..)
  , Command(..)
  , Instruction(..)
  , Assembly(..)
  , IOStateKeeping(..)
  , Context(..)
  , State(..)
  , IState(..)
  ) where

import qualified Data.Text as T
import Data.IORef (IORef)

import Rash.Array (Array, ArrayIO)


data RashPaths = RashPaths { pathDir :: FilePath
                           , pathASM :: FilePath
                           , pathState :: FilePath
                           }
               deriving (Show)

type ID = Int

data Part = TextPart T.Text
          | IDPart Bool ID
          deriving (Read, Show)

data Command = Command { commandParts :: Array Part
                       , assignStdin :: Maybe (Array Part)
                       }
         deriving (Read, Show)

data Instruction = Read { assignID :: ID
                        }
                   -- ^ Read a line into a register.

                 | Run { command :: Command
                       }
                   -- ^ Run a command with arguments.

                 | AssignRun { assignID :: ID
                             , command :: Command
                             }
                   -- ^ Run a command with arguments, and redirect its standard
                   -- out to a register.  Optionally, supply standard in.

                 | Assign { assignID :: ID
                          , contentParts :: Array Part
                          }
                   -- ^ Assign text to a register.

                 | JumpIfRetZero { jumpPos :: Int
                                 }
                   -- ^ Jump to an instruction position if the previous command
                   -- exited with return code 0.  If no command has previously
                   -- run, the return code is 0.

                 | Jump { jumpPos :: Int
                        }
                   -- ^ Jump to an instruction position.

                 | Exit
                   -- ^ Stop and exit.
              deriving (Read, Show)

data Assembly = Assembly (Array Instruction)
              deriving (Read, Show)

data IOStateKeeping = WriteAndReadFiles RashPaths
                    | InMemory (IORef (Maybe Assembly)) (IORef (Maybe IState)) (String -> IO ())

instance Show IOStateKeeping where
  show = const "<IOStateKeeping>"

data Context = Context { contextAssembly :: Assembly
                       , contextReadArgs :: T.Text
                       , contextIOStateKeeping :: IOStateKeeping
                       }
             deriving (Show)

data State = State { statePC :: Int
                   , stateVars :: ArrayIO T.Text
                   , stateJustRestarted :: Bool
                   , statePrevExitCode :: Int
                   }

data IState = IState { iStatePC :: Int
                     , iStateVars :: Array T.Text
                     }
            deriving (Read, Show)
