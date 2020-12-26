{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day23 where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Test.QuickCheck (Property, ioProperty, property)
import Test.QuickCheck.All (quickCheckAll)

type Cup = Int

type Cups = (Int, IntMap Cup)

move :: Cups -> Cups
move (n, m) =
  let n1 = m IntMap.! n
      n2 = m IntMap.! n1
      n3 = m IntMap.! n2
      next = m IntMap.! n3
      (maxN, _) = IntMap.findMax m
      descending = [n - 1, n - 2 .. 1] ++ [maxN, maxN - 1 ..]
      small = head $ filter (`notElem` [n1, n2, n3]) descending
      m' = IntMap.union (IntMap.fromList [(n, next), (small, n1), (n3, m IntMap.! small)]) m
   in (next, m')

takeAfter :: Int -> Int -> Cups -> [Cup]
takeAfter _ 0 _ = []
takeAfter i n cs@(_, m) = let c = m IntMap.! i in c : takeAfter c (n - 1) cs

labelsAfter100Moves :: [Cup] -> String
labelsAfter100Moves =
  concatMap show
    . takeAfter 1 8
    . (!! 100)
    . iterate move
    . cups

findStars :: [Cup] -> Int
findStars =
  product
    . takeAfter 1 2
    . (!! 10000000)
    . iterate move
    . cups
    . (++ [10 .. 1000000])

part1 :: IO ()
part1 = print . labelsAfter100Moves =<< input

part2 :: IO ()
part2 = print . findStars =<< input

-- Input handling

cups :: [Cup] -> Cups
cups = (,) <$> head <*> (IntMap.fromList . (zip <$> id <*> ((++) <$> tail <*> (: []) . head)))

readCups :: String -> [Cup]
readCups = map (read . (: []))

input :: IO [Cup]
input = readCups <$> readFile "data/Day23.txt"

-- Testing

prop_regression :: Property
prop_regression =
  ioProperty $
    (\cs -> labelsAfter100Moves cs == "34952786") <$> input

testCups :: [Cup]
testCups = readCups "389125467"

prop_labelsAfter100MovesUnit :: Property
prop_labelsAfter100MovesUnit = property $ labelsAfter100Moves testCups == "67384529"

prop_findStars :: Property
prop_findStars = property $ findStars testCups == 149245887792

return []

runTests :: IO Bool
runTests = $quickCheckAll
