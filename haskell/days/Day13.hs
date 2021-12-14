module Main
    ( main
    ) where

import           Advent                         ( splitOn )
import           Control.Arrow                  ( (&&&) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

data Folding = Foldx Int | Foldy Int deriving Show
data Manual = Manual
    { points   :: Set (Int, Int)
    , foldings :: [Folding]
    }
    deriving Show
type Input = Manual

foldPaper :: Set (Int, Int) -> Folding -> Set (Int, Int)
foldPaper s (Foldx v) =
    Set.map (\p@(x, y) -> if x > v then (2 * v - x, y) else p) s
foldPaper s (Foldy v) =
    Set.map (\p@(x, y) -> if y > v then (x, 2 * v - y) else p) s

visualize :: Set (Int, Int) -> String
visualize xs =
    unlines
        $ map (map (\(p, _) -> if p `elem` pointList then '#' else '.'))
        $ zipWith (\y arr -> zipWith (\x val -> ((x, y), val)) [0 ..] arr)
                  [0 ..]
                  (replicate w (replicate h '.'))
  where
    pointList = Set.toList xs
    w         = (+ 1) $ maximum $ map snd pointList
    h         = (+ 1) $ maximum $ map fst pointList

part1 :: Input -> Int
part1 m = Set.size $ foldPaper (points m) (head $ foldings m)

part2 :: Input -> String
part2 m = visualize $ foldl foldPaper (points m) (foldings m)

parseInstruction :: String -> Folding
parseInstruction xs | axis == "x" = Foldx val
                    | otherwise   = Foldy val
  where
    (axis, val) = (head &&& (read . last)) $ splitOn '=' $ last $ words xs

prepare :: String -> Input
prepare xs = Manual pointSet insttructionList
  where
    (pointList, instructions) = (head &&& last) $ splitOn "" $ lines xs
    pointSet = Set.fromList $ [ read ("(" ++ x ++ ")") | x <- pointList ]
    insttructionList = map parseInstruction instructions

main :: IO ()
main =
    readFile "inputs/input13.txt"
        >>= putStr
        .   (\(x, y) -> show x ++ "\n" ++ y)
        .   (part1 &&& part2)
        .   prepare
