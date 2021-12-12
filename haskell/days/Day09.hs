module Main
    ( main
    ) where

import           Control.Arrow                  ( (&&&) )
import           Data.Bifunctor                 ( second )
import           Data.Char                      ( digitToInt )
import           Data.List                      ( sort
                                                , sortOn
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

type Cave = Map (Int, Int) Int
type Input = Cave

colNum = 100
rowNum = 100

getNeighbours :: (Int, Int) -> [(Int, Int)]
getNeighbours (x, y) = filter
    (\(x, y) -> x < rowNum && x >= 0 && y < colNum && y >= 0)
    allNeighbours
    where allNeighbours = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

search :: Map (Int, Int) Int -> (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
search m p s
    | null neighboursNot9 = newS
    | otherwise = foldl
        (\c n -> if Set.member n c then c else Set.union c (search m n newS))
        Set.empty
        neighboursNot9
  where
    newS = Set.insert p s
    neighboursNot9 =
        filter (not . (`Set.member` s))
            $ filter (\k -> m Map.! k /= 9)
            $ getNeighbours p

part1 :: Input -> Int
part1 m =
    sum
        . map ((+ 1) . fst)
        . filter (\(val, ns) -> all (> val) ns)
        . map (second (map (m Map.!)))
        . Map.foldrWithKey (\k x ks -> (x, getNeighbours k) : ks) []
        $ m

part2 :: Input -> Int
part2 m =
    product
        . take 3
        . reverse
        . sort
        . map (Set.size . (\p -> search m p (Set.singleton p)))
        $ pits
  where
    pits =
        map fst
            . filter (\(p, val) -> all ((> val) . (m Map.!)) (getNeighbours p))
            . Map.toList
            $ m

prepare :: String -> Input
prepare =
    Map.fromList
        . concatMap (\(i, arr) -> zipWith (\x y -> ((i, x), y)) [0 ..] arr)
        . zip [0 ..]
        . map (map digitToInt)
        . lines


main :: IO ()
main = readFile "inputs/input09.txt" >>= print . (part1 &&& part2) . prepare
