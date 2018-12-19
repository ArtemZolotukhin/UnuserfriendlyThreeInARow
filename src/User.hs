module User where

import Field
import Control.Monad

control = replicateM_ 10 $ msg1 >> getLine >>=  putStrLn
	where msg1 = putStrLn "write coordinates and direction (ex: 1 2 l)"