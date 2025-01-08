module Rash.Array
  ( Array
  , ArrayIO
  , listToArray
  , sequenceToList
  , sequenceLength
  ) where

import qualified Data.Array.IArray as IA
import qualified Data.Array.IO as IOA


type Array a = IA.Array Int a
type ArrayIO a = IOA.IOArray Int a

listToArray :: [a] -> Array a
listToArray xs = IA.listArray (0, length xs - 1) xs

sequenceToList :: Array a -> [a]
sequenceToList = IA.elems

sequenceLength :: Array a -> Int
sequenceLength = (+ 1) . snd . IA.bounds
