module Field where

import Gems

generateGem::GemStone
generateGem = GemStone Red

generateRow:: Int -> [GemStone]
generateRow count = gen 0 count [] 
    where
        gen i n l = if (i < n) then
                        gen (i+1) n $ generateGem:l
                    else 
                        l
