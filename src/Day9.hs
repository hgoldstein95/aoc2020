{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day9 where

import Data.Foldable (find)
import Data.Maybe (fromJust)
import Test.QuickCheck (Property, ioProperty, property)
import Test.QuickCheck.All (quickCheckAll)

type Cypher = [Int]

type Window = Int

type Index = Int

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = ((x,) <$> xs) ++ pairs xs

focus :: Cypher -> Window -> Index -> ([Int], Int)
focus c w i = (take w . drop (i - w) $ c, c !! i)

valid :: Cypher -> Window -> Index -> Bool
valid c w i =
  let (xs, x) = focus c w i
   in any ((== x) . uncurry (+)) . pairs $ xs

firstInvalidIndex :: Window -> Cypher -> Index
firstInvalidIndex w c = fromJust . find (not . valid c w) $ [w ..]

firstInvalid :: Window -> Cypher -> Int
firstInvalid w c = c !! firstInvalidIndex w c

findWeakRange :: Cypher -> Index -> [Int]
findWeakRange c i = aux (take i c) []
  where
    target = c !! i
    aux [] w = w
    aux (x : xs) w
      | sum w == target = w
      | sum w < target = aux xs $ w ++ [x]
      | otherwise = aux (x : xs) $ drop 1 w

findWeakness :: Window -> Cypher -> Int
findWeakness w c = (\r -> maximum r + minimum r) . findWeakRange c $ firstInvalidIndex w c

part1 :: IO ()
part1 = print . firstInvalid 25 =<< input

part2 :: IO ()
part2 = print . findWeakness 25 =<< input

-- Input handling

readCypher :: String -> Cypher
readCypher = map read . lines

input :: IO Cypher
input = readCypher <$> readFile "data/Day9.txt"

-- Testing

prop_regression :: Property
prop_regression =
  ioProperty $
    ( \c ->
        firstInvalid 25 c == 167829540
          && findWeakness 25 c == 28045630
    )
      <$> input

testCypher :: Cypher
testCypher =
  readCypher
    "35\n\
    \20\n\
    \15\n\
    \25\n\
    \47\n\
    \40\n\
    \62\n\
    \55\n\
    \65\n\
    \95\n\
    \102\n\
    \117\n\
    \150\n\
    \182\n\
    \127\n\
    \219\n\
    \299\n\
    \277\n\
    \309\n\
    \576"

prop_validUnit :: Property
prop_validUnit = property $ firstInvalid 5 testCypher == 127

prop_findWeaknessUnit :: Property
prop_findWeaknessUnit = property $ findWeakness 5 testCypher == 62

return []

runTests :: IO Bool
runTests = $quickCheckAll