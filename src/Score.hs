module Score where

data ScoreType = ScoreType Int deriving (Show)

getValue:: ScoreType -> Int
getValue (ScoreType val) = val
