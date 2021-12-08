module Main
    ( main
    ) where
import           Control.Arrow                  ( (&&&) )

type Input = [Int]

solve :: (Int -> Int) -> Input -> Int
solve f xs = minimum
    (map (sum . map (f . abs) . (zipWith (+) xs . repeat)) [start .. end])
  where
    start = negate $ maximum xs
    end   = negate $ minimum xs

part1 :: Input -> Int
part1 = solve id

part2 :: Input -> Int
part2 = solve (\x -> div (x * (x + 1)) 2)

prepare :: String -> Input
prepare xs = read $ "[" ++ head (lines xs) ++ "]"

main :: IO ()
main = readFile "inputs/input07.txt" >>= print . (part1 &&& part2) . prepare
