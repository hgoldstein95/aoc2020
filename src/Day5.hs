{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Day5 where

import Control.Arrow (Arrow ((&&&)))
import Data.List (sort, unfoldr)
import Test.QuickCheck (Property, ioProperty, (.&&.), (===))
import Test.QuickCheck.All (quickCheckAll)

data Seat = Seat
  {row :: Int, col :: Int}
  deriving (Eq, Show)

seatID :: Seat -> Int
seatID (Seat r c) = r * 8 + c

-- For the record, I'm totally just trolling with `unfoldr`. The more reasonable way to write that
-- is `(2 ^) <$> [0 :: Int ..]` or even just adding it to the zipWith function.
fromBinary :: [Int] -> Int
fromBinary = sum . zipWith (*) (unfoldr (pure . (id &&& (* 2))) (1 :: Int)) . reverse

decodeChar :: Char -> Int
decodeChar c = if c `elem` ['F', 'L'] then 0 else 1

decode :: String -> Seat
decode s =
  let (r, c) = splitAt 7 . map decodeChar $ s
   in Seat (fromBinary r) (fromBinary c)

findMissing :: [Int] -> Int
findMissing (x : y : xs) = if x + 1 /= y then x + 1 else findMissing (y : xs)
findMissing _ = error "reached end"

part1 :: IO ()
part1 = print . maximum . map seatID =<< input

part2 :: IO ()
part2 = print . findMissing . sort . map seatID =<< input

-- Input handling

readSeats :: String -> [Seat]
readSeats = map decode . lines

input :: IO [Seat]
input = readSeats <$> readFile "data/Day5.txt"

-- Testing

prop_regression :: Property
prop_regression =
  ioProperty $
    ( \xs ->
        (maximum . map seatID) xs == 935
          && (findMissing . sort . map seatID) xs == 743
    )
      <$> input

prop_seatIDUnit :: Property
prop_seatIDUnit =
  seatID (Seat 70 7) === 567
    .&&. seatID (Seat 14 7) === 119
    .&&. seatID (Seat 102 4) === 820

prop_decodeUnit :: Property
prop_decodeUnit =
  decode "BFFFBBFRRR" === Seat 70 7
    .&&. decode "FFFBBBFRRR" === Seat 14 7
    .&&. decode "BBFFBBFRLL" === Seat 102 4

return []

runTests :: IO Bool
runTests = $quickCheckAll