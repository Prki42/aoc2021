module Main
    ( main
    ) where

import           Control.Arrow                  ( (&&&)
                                                , (***)
                                                )
import           Data.Char                      ( digitToInt )
import           Data.List                      ( group
                                                , maximumBy
                                                , minimumBy
                                                , sort
                                                , transpose
                                                )
import           Data.Ord                       ( comparing )

type Input = [String]

fromBinary :: String -> Int
fromBinary xs = sum $ zipWith dec (reverse xs) [0 .. length xs]
    where dec a b = digitToInt a * (2 ^ b)

common :: Input -> String
common xs = map (toBit . foldl (\c x -> c + digitToInt x) 0) . transpose $ xs
  where
    len = length xs
    toBit n = if n >= len `div` 2 then '1' else '0'

invertBin :: String -> String
invertBin xs = [ if x == '1' then '0' else '1' | x <- xs ]

gasWrap _ [x] = [x]
gasWrap f xs  = map (x :) (gasWrap f $ map tail $ filter ((== x) . head) xs)
    where x = head . f (comparing length) . group . sort $ map head xs

oxygen :: Input -> [String]
oxygen = gasWrap maximumBy

co2 :: Input -> [String]
co2 = gasWrap minimumBy

part1 :: Input -> Int
part1 xs = fromBinary (invertBin c) * fromBinary c where c = common xs

part2 :: Input -> Int
part2 = uncurry (*) . (parse *** parse) . (oxygen &&& co2)
    where parse = fromBinary . head

prepare :: String -> Input
prepare = lines

main :: IO ()
main = readFile "inputs/input03.txt" >>= print . (part1 &&& part2) . prepare
