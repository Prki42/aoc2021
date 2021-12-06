module Main
    ( main
    ) where

import           Advent                         ( splitOn )
import           Control.Arrow                  ( (&&&) )
import           Data.Function                  ( on )
import           Data.List                      ( group
                                                , groupBy
                                                , sort
                                                , sortBy
                                                )
import           Data.Ord                       ( comparing )

type Input = [Int]
type School = [(Int, Int)]

emptySchool :: School
emptySchool = zip [0 .. 8] $ repeat 0

mergeSchool :: School -> School
mergeSchool =
    map (foldl1 (\(v1, f1) (_, f2) -> (v1, f1 + f2)))
        . groupBy (on (==) fst)
        . sortBy (comparing fst)

dayUpdate :: School -> School
dayUpdate xs = mergeSchool $ (++) [(8, num68), (6, num68)] $ map
    (\(v, f) -> if v /= 0 then (v - 1, f) else (0, 0))
    xs
    where num68 = snd $ head $ filter ((== 0) . fst) xs

createSchool :: [Int] -> School
createSchool =
    mergeSchool . (++) emptySchool . map (head &&& length) . group . sort

run :: Int -> School -> Int
run n = sum . map snd . (!! n) . iterate dayUpdate

part1 :: Input -> Int
part1 = run 80 . createSchool

part2 :: Input -> Int
part2 = run 256 . createSchool

prepare :: String -> Input
prepare = map read . concatMap (splitOn ',') . lines

main :: IO ()
main = readFile "inputs/input06.txt" >>= print . (part1 &&& part2) . prepare
