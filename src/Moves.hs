module Moves where

data MoveType = MoveType Int deriving (Show)

getValue:: MoveType -> Int
getValue (MoveType val) = val
