module Field where

import Gems
import Control.Monad.Trans.Random
import Control.Monad
import Control.Monad.Random
import Data.List

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
--iterateField gemstoneField = iter gemstoneField 


--iterateFieldHorizontal::[[GemStone]] -> ([[GemStone]], [(Int, Int)])
--iterateFieldHorizontal gemField = 

iterateFieldHorizontal'::[[GemStone]] -> [(Int, Int)] -> (Int, Int) -> (Int, GemStoneType) -> ([[GemStone]], [(Int, Int)])
iterateFieldHorizontal' gemField toDelete (x, y) (matches, gemType) = 
    if x >= xlen then
        iterateFieldHorizontal' gemField toDelete
    where 
        xlen = length gemField
        ylen = length (gemField!!0)

hUpdateToDelete::[[GemStone]] -> [(Int, Int)] -> (Int, Int) -> matches -> [(Int, Int)]
hUpdateToDelete f toDel (x, y) m = m


replaceInListList:: (Int, Int) -> a -> [[a]] -> [[a]]
replaceInListList (i, j) val listList = replaceInList i (replaceInList j val $ listList !! i) listList

replaceInList:: Int -> a -> [a] -> [a]
replaceInList 0 new (x:xs) = new:xs
replaceInList i new list = (take i list) ++ (new:(drop (i+1) list ))



