module Field where

import Gems
import Control.Monad.Trans.Random
import Control.Monad
import Control.Monad.Random

-- instance Random GemStoneType where
--   randomR (lo,hi) g = ...
--   random g = ...

-- generateGem = GemStone <$> getRandom


generateGem::RandomGen g => Rand g GemStone
generateGem = GemStone <$> fromList [(Red,0.2),(Green,0.2),(Blue,0.2),(Orange,0.2),(Purple,0.2)]

generateRow:: RandomGen g => Int -> Rand g [GemStone]
generateRow count = replicateM count generateGem

generateField:: RandomGen g => Int -> Int -> Rand g [[GemStone]]
generateField rows columns = replicateM rows (generateRow columns)

--data WasChanges = Yes | No

--iterateField::[[GemStone]] -> [[GemStone]]
--iterateField =

replaceInListList:: (Int, Int) -> a -> [[a]] -> [[a]]
replaceInListList (i, j) val listList = replaceInList i (replaceInList j val $ listList !! i) listList

replaceInList:: Int -> a -> [a] -> [a]
replaceInList 0 new (x:xs) = new:xs
replaceInList i new list = (take i list) ++ (new:(drop (i+1) list ))

swapObj:: (Int, Int) -> (Int, Int) -> [[a]] -> [[a]]
swapObj (i1, j1) (i2, j2) gemField =
  let
    gem = gemField !! i1 !! j1
    in replaceInListList (i2, j2) gem (replaceInListList (i1, j1) (gemField !! i2 !! j2) gemField) 
