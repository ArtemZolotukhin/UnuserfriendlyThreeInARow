module User where

import Field
import Gems
import Control.Monad.Trans.Random
import Control.Monad
import Control.Monad.Random
import Data.List
import Data.List
import Data.Char

control gm = msg1 >> getLine >>= toInt -- >>= print
	where 
		msg1 = putStrLn "write coordinates (ex: 1 2 2 2)"
		toInt s = do
			let x = digitToInt (s !! 0) 
			let y = digitToInt (s !! 2) 
			let x1 = digitToInt (s !! 4) 
			let y1 = digitToInt (s !! 6) 
			return (swapObj (x, y) (x1, y1) gm)