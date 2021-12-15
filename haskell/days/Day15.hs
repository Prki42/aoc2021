module Main
    ( main
    ) where

import           Algorithm.Search               ( dijkstra )
import           Control.Arrow                  ( (&&&) )
import qualified Data.Bifunctor
import           Data.Char                      ( digitToInt )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust )

data Cave = Cave
    { cavern       :: Map (Int, Int) Int
    , rowNumber    :: Int
    , columnNumber :: Int
    }
    deriving Show
type Input = Cave

neighbours :: Cave -> (Int, Int) -> [(Int, Int)]
neighbours (Cave _ rows cols) (x, y) =
    let allNeighbours = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    in  filter (\(x1, y1) -> x1 >= 0 && x1 < cols && y1 >= 0 && y1 < rows)
               allNeighbours

traverseCave :: Cave -> Int
traverseCave c = fst $ fromJust $ dijkstra
    (neighbours c)
    (\_ p -> cavern c Map.! p)
    (== (rowNumber c - 1, columnNumber c - 1))
    (0, 0)

part1 :: Input -> Int
part1 = traverseCave

part2 :: Input -> Int
part2 = traverseCave

gridToMap :: [String] -> Cave
gridToMap xs = Cave
    ( Map.fromList
    $ concatMap
          (\(y, arr) -> zipWith (\x v -> ((x, y), digitToInt v)) [0 ..] arr)
    $ zip [0 ..] xs
    )
    (length xs)
    (length $ head xs)

prepare1 :: String -> Input
prepare1 = gridToMap . lines

prepare2 :: String -> Input
prepare2 s =
    (\(Cave c rows cols) -> Cave
            (Map.mapWithKey
                (\k x ->
                    (\d -> if d < 10 then d else ((d - 1) `mod` 9) + 1)
                        $ x
                        + (fst k `div` startCols)
                        + (snd k `div` startRows)
                )
                c
            )
            rows
            cols
        )
        $ gridToMap
        $ concat
        $ replicate 5
        $ map (concat . replicate 5) allLines
  where
    allLines  = lines s
    startRows = length allLines
    startCols = length $ head allLines

main :: IO ()
main =
    readFile "inputs/input15.txt"
        >>= print
        .   Data.Bifunctor.bimap part1 part2
        .   (prepare1 &&& prepare2)
