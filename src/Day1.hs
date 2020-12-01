{-# LANGUAGE TemplateHaskell #-}

module Day1 where

import Control.Monad (replicateM)
import Data.List (find, nub)
import Data.Maybe (fromJust)
import Test.QuickCheck (Property, ioProperty)
import Test.QuickCheck.All (quickCheckAll)

combinations :: Int -> [a] -> [[a]]
combinations n xs =
  map (map (xs !!)) . filter (\ys -> ys == nub ys) . replicateM n $ [0 .. length xs - 1]

find2020Product :: Int -> [Int] -> Int
find2020Product n = product . fromJust . find ((== 2020) . sum) . combinations n

part1 :: IO ()
part1 =
  print
    . find2020Product 2
    . parseInput
    =<< readInput

part2 :: IO ()
part2 =
  print
    . find2020Product 3
    . parseInput
    =<< readInput

prop_regression :: Property
prop_regression = ioProperty $ do
  xs <- parseInput <$> readInput
  pure $ find2020Product 2 xs == 121396 && find2020Product 3 xs == 73616634

readInput :: IO String
readInput = readFile "data/Day1.txt"

parseInput :: String -> [Int]
parseInput = map read . lines

return []

runTests :: IO Bool
runTests = $quickCheckAll