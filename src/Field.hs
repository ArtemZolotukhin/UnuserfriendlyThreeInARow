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
