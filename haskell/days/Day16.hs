module Main
    ( main
    ) where

import           Control.Arrow                  ( (&&&) )
import           Data.Char                      ( digitToInt
                                                , intToDigit
                                                )
import           Data.Functor                   ( (<&>) )
import           Numeric                        ( readHex
                                                , readInt
                                                , showIntAtBase
                                                )
import           Text.ParserCombinators.ReadP

data Packet = Literal Int Int | Operator Int Int [Packet] deriving Show
type Input = Packet

toBin :: (Integral a, Show a) => a -> String
toBin x = showIntAtBase 2 intToDigit x ""

toDec :: Integral a => String -> a
toDec = fst . head . readInt 2 (`elem` "01") digitToInt

padZeroes :: String -> String
padZeroes xs | len `mod` 4 == 0 = xs
             | otherwise        = replicate (4 - (len `mod` 4)) '0' ++ xs
    where len = length xs

bit :: ReadP Char
bit = choice [char '0', char '1']

bits :: Integral b => Int -> ReadP b
bits n = count n bit <&> toDec

packet :: ReadP Packet
packet = do
    version <- bits 3
    id      <- bits 3
    if id == 4
        then do
            s1 <- manyTill (count 5 bit) (char '0')
            s2 <- count 4 bit
            return $ Literal version (toDec . concat $ (tail <$> s1) ++ [s2])
        else do
            lenType <- bit
            if lenType == '0'
                then do
                    n         <- bits 15
                    packetStr <- count n bit
                    let packets =
                            fst . last $ readP_to_S (many1 packet) packetStr
                    return $ Operator version id packets
                else do
                    n       <- bits 11
                    packets <- count n packet
                    return $ Operator version id packets

versionSum :: Packet -> Int
versionSum (Literal v _    ) = v
versionSum (Operator v _ ls) = v + sum (map versionSum ls)

evaluatePacket :: Packet -> Int
evaluatePacket (Literal _ num) = num
evaluatePacket (Operator _ packetId ls') =
    let ls = map evaluatePacket ls'
    in  case packetId of
            0 -> sum ls
            1 -> product ls
            2 -> minimum ls
            3 -> maximum ls
            5 -> if head ls > (ls !! 1) then 1 else 0
            6 -> if head ls < (ls !! 1) then 1 else 0
            7 -> if head ls == (ls !! 1) then 1 else 0
            _ -> 0

part1 :: Input -> Int
part1 = versionSum

part2 :: Input -> Int
part2 = evaluatePacket

prepare :: String -> Input
prepare xs =
    fst
        . last
        . readP_to_S packet
        . padZeroes
        . (zeroes ++)
        . toBin
        . fst
        . head
        . readHex
        $ xs
    where zeroes = replicate (4 * length (takeWhile (== '0') xs)) '0'


main :: IO ()
main = readFile "inputs/input16.txt" >>= print . (part1 &&& part2) . prepare
