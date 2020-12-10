{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day10 where

import Control.Monad.Trans.State (evalState, get, modify)
import Data.List (sort)
import Data.Map ((!?))
import qualified Data.Map as Map
import Test.QuickCheck (Property, ioProperty, property)
import Test.QuickCheck.All (quickCheckAll)

type Jolts = [Integer]

differences :: [Integer] -> [Integer]
differences (x : y : xs) = y - x : differences (y : xs)
differences _ = []

padChain :: [Integer] -> [Integer]
padChain xs = 0 : xs ++ [last xs + 3]

examineChain :: Jolts -> Integer
examineChain =
  toInteger
    . (\xs -> length (filter (== 1) xs) * length (filter (== 3) xs))
    . differences
    . padChain

memoFix :: Ord a => (forall m. Monad m => (a -> m b) -> a -> m b) -> a -> b
memoFix compute = (`evalState` Map.empty) . aux
  where
    aux x = get >>= maybe (computeAndSave x) pure . (!? x)
    computeAndSave x = do
      k <- compute aux x
      modify (Map.insert x k)
      pure k

countArrangements :: Jolts -> Integer
countArrangements = (`div` 2) . memoFix aux . (0,)
  where
    aux f (prev, x : xs)
      | prev + 3 < x = pure 0
      | otherwise = (+) <$> f (prev, xs) <*> f (x, xs)
    aux _ (_, []) = pure 1

part1 :: IO ()
part1 = print . examineChain =<< input

part2 :: IO ()
part2 = print . countArrangements =<< input

-- Input handling

readJolts :: String -> Jolts
readJolts = sort . map read . lines

input :: IO Jolts
input = readJolts <$> readFile "data/Day10.txt"

-- Testing

prop_regression :: Property
prop_regression =
  ioProperty $
    (\js -> examineChain js == 1625 && countArrangements js == 3100448333024) <$> input

testJolts1 :: Jolts
testJolts1 = readJolts "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4"

testJolts2 :: Jolts
testJolts2 =
  readJolts
    "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n\
    \19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3"

prop_examineChainUnit :: Property
prop_examineChainUnit =
  property $
    examineChain testJolts1 == 35 && examineChain testJolts2 == 220

prop_countArrangementsUnit :: Property
prop_countArrangementsUnit =
  property $
    countArrangements testJolts1 == 8 && countArrangements testJolts2 == 19208

return []

runTests :: IO Bool
runTests = $quickCheckAll