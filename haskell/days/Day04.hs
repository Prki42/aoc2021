{-# LANGUAGE TupleSections #-}
module Main
    ( main
    ) where

import           Advent                         ( splitOn )
import           Control.Arrow                  ( (&&&) )
import           Data.List                      ( transpose )

type Field = (Bool, Int)
type Board = [[Field]]
data Bingo = Bingo
    { chosen :: [Int]
    , boards :: [Board]
    }
    deriving (Show, Read)
type Input = Bingo

isMarkedFull :: Board -> Bool
isMarkedFull = uncurry (||) . (or . op &&& or . op . transpose)
    where op = map (all fst)

calcSum :: Board -> Int
calcSum = sum . concatMap (map snd . filter (not . fst))

mark :: Bingo -> Int -> Bingo
mark b num = Bingo
    (chosen b)
    (map
        (map (map (\f@(marked, val) -> if val == num then (True, val) else f)))
        bs
    )
    where bs = boards b

playBingo :: Int -> Bingo -> Int
playBingo idx b
    | null winner = playBingo (idx + 1) marked
    | otherwise   = (* num) $ calcSum $ (!!) (boards marked) $ fst $ head winner
  where
    num    = chosen b !! idx
    marked = mark b num
    winner = filter snd $ zip [0 ..] $ map isMarkedFull $ boards marked

filterBoardsByIndex :: [Int] -> Bingo -> Bingo
filterBoardsByIndex idxs b = Bingo
    (chosen b)
    (map snd $ filter (not . flip elem idxs . fst) $ zip [0 ..] $ boards b)

playUntilLastWin :: Int -> Bingo -> (Board, Int) -> Int
playUntilLastWin idx b (br, num2)
    | idx == length (chosen b) = calcSum br * num2
    | null winner = playUntilLastWin (idx + 1) marked (br, num2)
    | otherwise = playUntilLastWin
        (idx + 1)
        (filterBoardsByIndex (map fst winner) marked)
        ((!!) (boards marked) $ fst $ last winner, num)
  where
    num    = chosen b !! idx
    marked = mark b num
    winner = filter snd $ zip [0 ..] $ map isMarkedFull $ boards marked

part1 :: Input -> Int
part1 = playBingo 0

part2 :: Input -> Int
part2 b = playUntilLastWin 0 b (head $ boards b, 0)

prepare :: String -> Input
prepare xs = Bingo
    (map read . splitOn ',' . head $ splitted)
    (map (map (map ((False, ) . read) . words)) $ splitOn "" $ drop 2 splitted)
    where splitted = lines xs

main :: IO ()
main = readFile "inputs/input04.txt" >>= print . (part1 &&& part2) . prepare
