module Rash.SequenceUtilities
  ( Sequence
  , SequenceIO
  , listToSequence
  , sequenceToList
  , sequenceLength
  ) where

import qualified Data.Array.IArray as IA
import qualified Data.Array.IO as IOA


type Sequence a = IA.Array Int a
type SequenceIO a = IOA.IOArray Int a

listToSequence :: [a] -> Sequence a
listToSequence xs = IA.listArray (0, length xs - 1) xs

sequenceToList :: Sequence a -> [a]
sequenceToList = IA.elems

sequenceLength :: Sequence a -> Int
sequenceLength = (+ 1) . snd . IA.bounds
