module Advent
    ( splitOn
    ) where

splitOn :: Eq t => t -> [t] -> [[t]]
splitOn p s = case dropWhile (== p) s of
    [] -> []
    s' -> w : splitOn p s'' where (w, s'') = break (== p) s'
