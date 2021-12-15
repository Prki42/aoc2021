module Main
    ( main
    ) where

import           Control.Arrow                  ( (&&&) )
import           Data.List                      ( group
                                                , sort
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

data PolymerData = PolymerData
    { template       :: String
    , insertionRules :: Map String String
    }
    deriving Show
type Input = PolymerData

subLists :: [a] -> [[a]]
subLists [] = []
subLists xs = take 2 xs : subLists (drop 1 xs)

runNSteps :: Int -> PolymerData -> Map Char Int
runNSteps n (PolymerData t r) = snd $ last $ take (n + 1) $ iterate
    (step r)
    startState
  where
    startState =
        ( Map.fromList
            $ map (\x -> (head x, length x))
            $ group
            $ sort
            $ filter (\x -> length x == 2)
            $ subLists t
        , Map.fromList $ map (\x -> (head x, length x)) $ group $ sort t
        )
    step
        :: Map String String
        -> (Map String Int, Map Char Int)
        -> (Map String Int, Map Char Int)
    step rules m = (newCombs, counter)
      where
        state   = fst m
        counter = Map.foldrWithKey
            (\k x c ->
                Map.unionWith (+) c $ Map.singleton (head $ rules Map.! k) x
            )
            (snd m)
            state
        newCombs = Map.foldrWithKey
            (\k x c -> if x == 0
                then c
                else
                    Map.unionWith (+) c
                    $ Map.fromList
                    $ (\p -> [(head k : [p], x), (p : [last k], x)])
                    $ head (rules Map.! k)
            )
            Map.empty
            state

part1 :: Input -> Int
part1 =
    uncurry (-) . (maximum &&& minimum) . map snd . Map.toList . runNSteps 10

part2 :: Input -> Int
part2 =
    uncurry (-) . (maximum &&& minimum) . map snd . Map.toList . runNSteps 40

parseRule :: String -> Map String String
parseRule = (\(x, y) -> Map.fromList [(x, y)]) . (head &&& last) . words

prepare :: String -> Input
prepare xs = PolymerData template ruleMap
  where
    (template, rules) = (head &&& drop 1 . tail) $ lines xs
    ruleMap = foldl (\c x -> Map.union c $ parseRule x) Map.empty rules

main :: IO ()
main = readFile "inputs/input14.txt" >>= print . (part1 &&& part2) . prepare
