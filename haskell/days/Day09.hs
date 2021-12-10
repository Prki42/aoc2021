module Main
    ( main
    ) where

import           Control.Arrow                  ( (&&&) )
import           Data.Bifunctor                 ( Bifunctor(second) )
import           Data.Char                      ( digitToInt )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

data Cave = Cave
    { grid   :: Map Int Int
    , rowNum :: Int
    , colNum :: Int
    }
type Input = Cave

neighbours :: Int -> Int -> Int -> [Int]
neighbours n rowN colN
    | modCol > 0 && modCol < colN - 1 = (n + 1) : (n - 1) : upDown
    | modCol > 0                      = (n - 1) : upDown
    | otherwise                       = (n + 1) : upDown
  where
    upDown = filter (\n -> n >= 0 && n < rowN * colN) [n + colN, n - colN]
    modCol = n `mod` colN


part1 :: Input -> Int
part1 (Cave m rowN colN) = sum $ map (+ 1) a
  where
    a = map fst $ filter (\(val, ns) -> all (> val) ns) $ map
        ( Data.Bifunctor.second (map (\n -> Map.findWithDefault 0 n m))
        . (\(p, val) -> (val, neighbours p rowN colN))
        )
        t
    t = Map.toList m

part2 :: Input -> Int
part2 = const 2

prepare :: String -> Input
prepare xs = Cave (Map.fromList $ zip [0 ..] g) (length rows) colN
  where
    rows = lines xs
    colN = length . head $ rows
    g    = concatMap (map digitToInt) rows


main :: IO ()
main = readFile "inputs/input09.txt" >>= print . (part1 &&& part2) . prepare
