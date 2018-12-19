module User where

import Control.Monad

control 0 = return ()
control n = do
  putStrLn "write coordinates and direction (ex: 1 2 l)"
  getLine
  control (n-1)

main = control 10