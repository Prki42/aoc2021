module Advent (splitOn) where

splitOn :: Char -> String -> [String]
splitOn p s = case dropWhile (== p) s of
    "" -> []
    s' -> w : splitOn p s''
        where
            (w, s'') = break (==p) s'