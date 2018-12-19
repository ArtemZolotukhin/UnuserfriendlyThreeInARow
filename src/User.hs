module User where

import Field
import Gems
import Control.Monad.Trans.Random
import Control.Monad
import Control.Monad.Random
import Data.List
import Data.List
import Data.Char

gener = mkStdGen 42
rrr = evalRand (generateField 5 5) gener

control rrr = replicateM_ 2 $ msg1 >> getLine >>= toInt 
	where 
		msg1 = putStrLn "write coordinates (ex: 1 2 2 2)"
		toInt s = do
			--- let rrr = [[1,2,3], [3,2,1], [1,0,1]]
			let z = digitToInt (s !! 2) 
			let x1 = digitToInt (s !! 4) 
			let y1 = digitToInt (s !! 6) 
			let x = digitToInt (s !! 0) 
			print $ replaceInToRandom (x, z) rrr
--		doubleMe :: Integer -> Integer
--		doubleMe x = x + x  
-- 
