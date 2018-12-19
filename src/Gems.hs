module Gems where

data GemStoneType = Red | Green | Blue | Orange | Purple | Nothing

data GemStone = GemStone GemStoneType


instance Show GemStoneType where
    show Red = "R"
    show Green = "G"
    show Blue = "B"
    show Orange = "O"
    show Purple = "P"

instance Show(GemStone) where
    show (GemStone t) = show t



