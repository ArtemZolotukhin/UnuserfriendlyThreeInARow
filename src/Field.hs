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


generateGem:: RandomGen g => Rand g GemStone
generateGem = GemStone <$> fromList [(Red,0.2),(Green,0.2),(Blue,0.2),(Orange,0.2),(Purple,0.2)]

generateRow:: RandomGen g => Int -> Rand g [GemStone]
generateRow count = replicateM count generateGem

generateField:: RandomGen g => Int -> Int -> Rand g [[GemStone]]
generateField rows columns = replicateM rows (generateRow columns)

--data WasChanges = Yes | No


execField::[[GemStone]]->[[GemStone]]
execField field = foldr (\(x,y) iField -> replaceInToRandom (x,y) iField) field (iterateField field)

iterateField::[[GemStone]] -> [(Int, Int)]
iterateField gemstoneField = (iterateCol [] 0) ++ (iterateR [] 0)
    where 
        iterateCol toDelete x =
            if (x < length gemstoneField) then 
                iterateCol (toDelete ++ (map (\intVal -> (x, intVal)) (iterateRow (gemstoneField !! x)))  ) (x+1)
            else 
                toDelete
        iterateR toDelete y = 
            if (y < length (gemstoneField !! 0)) then 
                iterateR (toDelete ++ (map (\intVal -> (intVal, y)) (iterateRow (row gemstoneField y)))  ) (y+1)
            else 
                toDelete



row::[[GemStone]] -> Int -> [GemStone]
row gemField y = iter 0 ([]::[GemStone])
    where
        iter i gems = if i < (length gemField) then
                                iter (i+1) (((gemField !! i) !! y):gems)
                              else
                                 gems

--return gems to delete
iterateRow::[GemStone] -> [Int]
iterateRow stones = iterate ([]::[Int]) 0
    where iterate s i = if i < ((length stones) - 3) then
                            if (foldr (foldrFunc) (gemStoneTypeByGemStone (part!!0)) part) /= Gems.Nothing then
                                iterate (i:(i+1):(i+2):s) (i+3)
                            else
                                iterate s (i+1)
                        else
                            s
                        where
                            part::[GemStone]
                            part = (take 3 (drop i stones))


foldrFunc::GemStone -> GemStoneType -> GemStoneType
foldrFunc stone stoneType = if (gemStoneTypeByGemStone stone) /= stoneType then Gems.Nothing else stoneType

gemStoneTypeByGemStone::GemStone -> GemStoneType
gemStoneTypeByGemStone (GemStone stoneType) = stoneType

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

replaceInToRandom:: (Int, Int) -> [[GemStone]] -> [[GemStone]]
replaceInToRandom (i1, j1) gemField2 =
  let
    gener = mkStdGen 41
    in replaceInListList (i1, j1) (evalRand (generateGem) gener) gemField2

generateTrue:: (Int)-> [[GemStone]]
generateTrue (size) =
  let
    gener = mkStdGen 13
    in evalRand (generateField size size) gener
