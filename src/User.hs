module User where

import Field
import Gems
import Control.Monad.Trans.Random
import Control.Monad
import Control.Monad.Random
import Data.List
import Data.List
import Data.Char

rrr = generateTrue 5

control gm = replicateM_ 5 $ printM >> msg1 >> getLine >>= toInt >>= control
	where
		printM = print gm
		msg1 = putStrLn "write coordinates (ex: 1 2 2 2)"
		toInt s = do
			let x = digitToInt (s !! 0) 
			let y = digitToInt (s !! 2) 
			let x1 = digitToInt (s !! 4) 
			let y1 = digitToInt (s !! 6) 
			-- print $ swapObj (x, y) (x1, y1) gm
			return (execField(swapObj (x, y) (x1, y1) gm))

start = control rrr