module Main
    ( main
    ) where

import           Control.Arrow                  ( (&&&) )

type Input = [Int]

filterIncrease :: (Ord a, Num a) => [a] -> Int
filterIncrease all@(_ : xs) = length $ filter (== True) $ zipWith (>) xs all
filterIncrease []           = 0

part1 :: Input -> Int
part1 = filterIncrease

subLists :: [a] -> [[a]]
subLists [] = []
subLists xs = take 3 xs : subLists (drop 1 xs)

part2 :: Input -> Int
part2 = filterIncrease . map sum . subLists

prepare :: String -> Input
prepare = map read . lines

main :: IO ()
main = readFile "inputs/input01.txt" >>= print . (part1 &&& part2) . prepare
