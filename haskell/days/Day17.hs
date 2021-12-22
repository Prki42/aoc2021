{-# LANGUAGE TupleSections #-}
module Main
    ( main
    ) where

import           Advent                         ( splitOn )
import           Control.Arrow                  ( (&&&) )
import           Data.List                      ( groupBy )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Ord                       ( comparing )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set


data Area = Area
    { xc1 :: Int
    , xc2 :: Int
    , yc1 :: Int
    , yc2 :: Int
    }
    deriving Show

type Input = Area

vyUpValues :: Area -> [Int]
vyUpValues (Area _ _ ymin _) = let y = abs ymin in [0 .. y - 1]

vyUpSteps :: Int -> Area -> [Int]
vyUpSteps vy (Area _ _ ymin ymax) = map (+ (2 * vy + 1)) [start .. end]
  where
    dk1   = floor $ sqrt $ fromIntegral $ (2 * vy + 1) ^ 2 - 8 * ymin
    dk2   = ceiling $ sqrt $ fromIntegral $ (2 * vy + 1) ^ 2 - 8 * ymax
    start = ceiling $ (-2 * fromIntegral vy - 1 + fromIntegral dk2) / 2
    end   = floor $ (-2 * fromIntegral vy - 1 + fromIntegral dk1) / 2

vyDownValues :: Area -> [Int]
vyDownValues (Area _ _ ymin _) = [ymin .. (-1)]

vyDownSteps :: Int -> Area -> [Int]
vyDownSteps vy (Area _ _ ymin ymax) = [start .. end]
  where
    dk1   = floor $ sqrt $ fromIntegral $ (2 * vy + 1) ^ 2 - 8 * ymin
    dk2   = ceiling $ sqrt $ fromIntegral $ (2 * vy + 1) ^ 2 - 8 * ymax
    start = ceiling $ (2 * fromIntegral vy + 1 + fromIntegral dk2) / 2
    end   = floor $ (2 * fromIntegral vy + 1 + fromIntegral dk1) / 2

vxConvValues :: Area -> [Int]
vxConvValues (Area xmin xmax _ _) = [start .. end]
  where
    start = ceiling $ (-1 + sqrt (1 + 8 * fromIntegral xmin)) / 2
    end   = floor $ (-1 + sqrt (1 + 8 * fromIntegral xmax)) / 2

vxConvSteps :: Int -> Area -> [Int]
vxConvSteps vx (Area xmin xmax ymin _) = [start .. end]
  where
    dk1   = floor $ sqrt $ fromIntegral $ (2 * vx + 1) ^ 2 - 8 * xmin
    start = ceiling $ (2 * fromIntegral vx + 1 - fromIntegral dk1) / 2
    end   = 2 * abs ymin + 2

vxNonConvValues :: Area -> [Int]
vxNonConvValues (Area xmin xmax _ _) = [start .. xmax]
    where start = floor $ (-1 + sqrt (1 + 8 * fromIntegral xmax)) / 2

vxNonConvSteps :: Int -> Area -> [Int]
vxNonConvSteps vx (Area xmin xmax _ _) = [start .. end]
  where
    dk1   = floor $ sqrt $ fromIntegral $ (2 * vx + 1) ^ 2 - 8 * xmin
    dk2   = ceiling $ sqrt $ fromIntegral $ (2 * vx + 1) ^ 2 - 8 * xmax
    start = ceiling $ (2 * fromIntegral vx + 1 - fromIntegral dk1) / 2
    end   = floor $ (2 * fromIntegral vx + 1 - fromIntegral dk2) / 2

part1 :: Input -> Int
part1 (Area _ _ ymin _) = let y = abs ymin - 1 in (y * (y + 1)) `div` 2

part2 :: Input -> Int
part2 a@(Area xmin xmax ymin ymax) =
    Set.size $ Set.fromList $ concatMap snd $ Map.toList $ Map.intersectionWith
        (\p1 p2 -> concatMap (\x -> map (x, ) p2) p1)
        xk
        yk
  where
    yk =
        Map.fromListWith (++)
            $  concatMap (\(vy, ks) -> map (, [vy]) ks)
            $  map (\p -> (p, p `vyUpSteps` a))   (vyUpValues a)
            ++ map (\p -> (p, p `vyDownSteps` a)) (vyDownValues a)
    xk =
        Map.fromListWith (++)
            $  concatMap (\(vx, ks) -> map (, [vx]) ks)
            $  map (\p -> (p, p `vxConvSteps` a))    (vxConvValues a)
            ++ map (\p -> (p, p `vxNonConvSteps` a)) (vxNonConvValues a)

prepare :: String -> Input
prepare =
    (\x -> uncurry (uncurry Area (fst x)) (snd x))
        . (extract . init . head &&& extract . last)
        . drop 2
        . words
        . head
        . lines
    where extract = (read . head &&& read . last) . splitOn '.' . drop 2

main :: IO ()
main = readFile "inputs/input17.txt" >>= print . (part1 &&& part2) . prepare
