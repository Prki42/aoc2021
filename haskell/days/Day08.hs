module Main
    ( main
    ) where

import           Advent                         ( splitOn )
import           Control.Arrow                  ( (&&&) )
import           Data.List                      ( elemIndex
                                                , group
                                                , sort
                                                , sortOn
                                                )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

import           Data.Maybe                     ( mapMaybe )

type Row = ([String], [String])
type Input = [Row]

-- freqMap = Map.fromList [(6, 'b'), (4, 'e'), (9, 'f')]

decode :: Row -> Int
decode (allDigits, wanted) = foldl (\c x -> c * 10 + x) 0
    $ mapMaybe (flip elemIndex allDigitsSorted . sort) wanted
  where
    [e, b, f] =
        map (Set.fromList . (\x -> [head x]))
            $ filter (\x -> length x `elem` [6, 4, 9])
            $ sortOn length
            $ group
            $ sort
            $ concat allDigits
    [s1, s7, s4, _, _, _, _, _, _, s8] =
        map Set.fromList $ sortOn length allDigits
    c               = Set.difference s1 f
    a               = Set.difference s7 s1
    d               = Set.difference (Set.difference s4 s7) b
    g               = Set.difference (Set.difference s8 (Set.union a s4)) e
    s0              = foldl Set.union a [b, c, e, f, g]
    s2              = foldl Set.union a [c, d, e, g]
    s3              = foldl Set.union a [c, d, f, g]
    s5              = foldl Set.union a [b, d, f, g]
    s6              = foldl Set.union a [b, d, e, f, g]
    s9              = foldl Set.union a [b, c, d, f, g]
    allDigitsSorted = map Set.toList [s0, s1, s2, s3, s4, s5, s6, s7, s8, s9]


part1 :: Input -> Int
part1 = length . filter (`elem` [2, 3, 4, 7]) . map length . concatMap snd

part2 :: Input -> Int
part2 = sum . map decode

prepare :: String -> Input
prepare = map ((head &&& last) . map words . splitOn '|') . lines

main :: IO ()
main = readFile "inputs/input08.txt" >>= print . (part1 &&& part2) . prepare
