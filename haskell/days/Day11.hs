module Main
    ( main
    ) where

import           Control.Arrow                  ( (&&&) )
import           Data.Char                      ( digitToInt )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

type SquidGrid = Map (Int, Int) Int
type Input = SquidGrid

getNeighbours :: (Int, Int) -> [(Int, Int)]
getNeighbours (x, y) = filter
    (\(x, y) -> x < 10 && x >= 0 && y < 10 && y >= 0)
    allNeighbours
  where
    allNeighbours =
        [ (x + 1, y + 1)
        , (x + 1, y)
        , (x + 1, y - 1)
        , (x - 1, y)
        , (x - 1, y + 1)
        , (x - 1, y - 1)
        , (x    , y + 1)
        , (x    , y - 1)
        ]

incrementAll :: SquidGrid -> SquidGrid
incrementAll = Map.map (+ 1)

updateNeighbours :: SquidGrid -> SquidGrid
updateNeighbours m = foldl
    (flip $ Map.adjust (\x -> if x == 0 then 0 else x + 1))
    newM
    toUpdate
  where
    newM = Map.map (\x -> if x > 9 then 0 else x) m
    toUpdate =
        concatMap getNeighbours
            $ Map.foldrWithKey (\k x ks -> k : ks) []
            $ Map.filter (> 9) m

updateNeighboursRec :: SquidGrid -> SquidGrid
updateNeighboursRec m | newGrid == m = m
                      | otherwise    = updateNeighboursRec newGrid
    where newGrid = updateNeighbours m

resetLight :: SquidGrid -> SquidGrid
resetLight = Map.map (\x -> if x > 9 then 0 else x)

countZeros :: SquidGrid -> Int
countZeros = Map.size . Map.filter (== 0)

simulateStep :: SquidGrid -> (Int, SquidGrid)
simulateStep = (countZeros &&& id) . updateNeighboursRec . incrementAll

runSim :: Int -> Int -> SquidGrid -> Int
runSim 0  n _ = n
runSim it n m = runSim (it - 1) (n + zeroCount) newM
    where (zeroCount, newM) = simulateStep m

findAllFlash :: Int -> SquidGrid -> Int
findAllFlash n m | zeroCount == 100 = n
                 | otherwise        = findAllFlash (n + 1) newM
    where (zeroCount, newM) = simulateStep m


part1 :: Input -> Int
part1 = runSim 100 0

part2 :: Input -> Int
part2 = findAllFlash 1

prepare :: String -> Input
prepare =
    Map.fromList
        . concatMap (\(i, arr) -> zipWith (\x y -> ((i, x), y)) [0 ..] arr)
        . zip [0 ..]
        . map (map digitToInt)
        . lines

main :: IO ()
main = readFile "inputs/input11.txt" >>= print . (part1 &&& part2) . prepare
