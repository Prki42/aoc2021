module Main
    ( main
    ) where

import           Control.Arrow                  ( (&&&) )
import           Data.List                      ( sort )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

type Input = [String]

pairs :: Map Char Char
pairs = Map.fromList [('{', '}'), ('(', ')'), ('[', ']'), ('<', '>')]

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score _   = 0

score2 :: Char -> Int
score2 ')' = 1
score2 ']' = 2
score2 '}' = 3
score2 '>' = 4
score2 _   = 0

stackSolver :: [Char] -> [Char] -> Int -> (Int, [Char])
stackSolver []       ys n = (n, ys)
stackSolver (x : xs) [] n = stackSolver xs ys newScore
  where
    isOpening = Map.member x pairs
    newScore  = if isOpening then n else n + score x
    ys        = [ Map.findWithDefault ')' x pairs | isOpening ]
stackSolver (x : xs) (y : ys) n
    | isOpening = stackSolver xs (closing : y : ys) n
    | y == x    = stackSolver xs ys n
    | otherwise = stackSolver xs ys (n + score x)
  where
    isOpening = Map.member x pairs
    closing   = Map.findWithDefault ')' x pairs
    newScore  = if isOpening then n else n + score x

part1 :: Input -> Int
part1 = sum . map (fst . (\x -> stackSolver x [] 0))

part2 :: Input -> Int
part2 =
    (\(len, xs) -> xs !! ((len - 1) `div` 2))
        . (length &&& id)
        . sort
        . map (foldl (\c x -> 5 * c + score2 x) 0 . snd)
        . filter ((== 0) . fst)
        . map (\x -> stackSolver x [] 0)

prepare :: String -> Input
prepare = lines

main :: IO ()
main = readFile "inputs/input10.txt" >>= print . (part1 &&& part2) . prepare
