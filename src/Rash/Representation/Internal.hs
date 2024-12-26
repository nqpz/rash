module Rash.Representation.Internal
  ( RashPaths(..)
  , ID
  , Part(..)
  , Command(..)
  , Instruction(..)
  , Assembly(..)
  , Context(..)
  , State(..)
  , IState(..)
  ) where

import qualified Data.Text as T

import Rash.SequenceUtilities (Sequence, SequenceIO)


data RashPaths = RashPaths { pathDir :: FilePath
                           , pathASM :: FilePath
                           , pathState :: FilePath
                           }
               deriving (Show)

type ID = Int

data Part = TextPart T.Text
          | IDPart Bool ID
          deriving (Read, Show)

data Command = Command { commandParts :: Sequence Part
                       , assignStdin :: Maybe (Sequence Part)
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
                          , contentParts :: Sequence Part
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

data Assembly = Assembly (Sequence Instruction)
              deriving (Read, Show)

data Context = Context { contextAssembly :: Assembly
                       , contextReadArgs :: T.Text
                       , contextPaths :: RashPaths
                       }
             deriving (Show)

data State = State { statePC :: Int
                   , stateVars :: SequenceIO T.Text
                   , stateJustRestarted :: Bool
                   , statePrevExitCode :: Int
                   }

data IState = IState { iStatePC :: Int
                     , iStateVars :: Sequence T.Text
                     }
            deriving (Read, Show)
