module Main
    ( main
    ) where

import           Advent                         ( splitOn )
import           Control.Arrow                  ( (&&&) )
import           Data.Char                      ( toUpper )

data Movement = Up | Down | Forward deriving (Show, Read)
data Command = Command Movement Int
    deriving (Show, Read)

type Submarine = (Int, Int, Int)
type Input = [Command]

fst' :: (a, a, a) -> a
fst' (x, _, _) = x

snd' :: (a, a, a) -> a
snd' (_, x, _) = x

move1 :: Command -> Submarine -> Submarine
move1 (Command Up      n) (d, h, a) = (d - n, h, a)
move1 (Command Down    n) (d, h, a) = (d + n, h, a)
move1 (Command Forward n) (d, h, a) = (d, h + n, a)

move2 :: Command -> Submarine -> Submarine
move2 (Command Up      n) (d, h, a) = (d, h, a - n)
move2 (Command Down    n) (d, h, a) = (d, h, a + n)
move2 (Command Forward n) (d, h, a) = (d + a * n, h + n, a)

part1 :: Input -> Int
part1 = uncurry (*) . (fst' &&& snd') . foldl (flip move1) (0, 0, 1)

part2 :: Input -> Int
part2 = uncurry (*) . (fst' &&& snd') . foldl (flip move2) (0, 0, 0)

prepare :: String -> Input
prepare x =
    [ Command (read (toUpper (head c) : tail c) :: Movement) (read n :: Int)
    | [c, n] <- splitted
    ]
    where splitted = map (splitOn ' ') $ splitOn '\n' x

main :: IO ()
main = readFile "inputs/input02.txt" >>= print . (part1 &&& part2) . prepare
