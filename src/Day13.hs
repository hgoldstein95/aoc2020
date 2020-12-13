{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day13 where

import Control.Arrow ((&&&))
import Control.Lens (makeLenses, view, (^.))
import Data.Foldable (minimumBy)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromJust)
import Data.Ord (comparing)
import Data.Tuple (swap)
import Math.NumberTheory.Moduli (chinese)
import Test.QuickCheck (Property, ioProperty, property)
import Test.QuickCheck.All (quickCheckAll)

type Time = Integer

type Bus = Integer

data Notes = Notes {_earliest :: Time, _busses :: [Maybe Bus]}
  deriving (Eq, Show)

makeLenses ''Notes

type Constraint = (Time, Bus)

constraints :: Notes -> [Constraint]
constraints = catMaybes . zipWith (\x y -> (x,) <$> y) [0 ..] . view busses

timeTillNext :: Notes -> Bus -> Time
timeTillNext n b = abs $ (b - n ^. earliest) `mod` b

nextBus :: Notes -> (Bus, Time)
nextBus n = minimumBy (comparing snd) . map (id &&& timeTillNext n) . catMaybes $ n ^. busses

-- | Given constraints [(t1, Just x), (t2, Nothing), (t3, Just y)], finds the earliest timestamp t
-- | such that bus x leaves at time (t + t1), bus y leaves at time (t + t3), etc.
solve :: [Constraint] -> Time
solve = uncurry mod . swap . foldr1 (\m n -> (fromJust (chinese m n), snd m * snd n))

part1 :: IO ()
part1 = print . uncurry (*) . nextBus =<< input

part2 :: IO ()
part2 = print . solve . constraints =<< input

-- Input handling

readNotes :: String -> Notes
readNotes s =
  let (e : bs : _) = lines s
   in Notes (read e) (map readBus $ splitOn "," bs)
  where
    readBus "x" = Nothing
    readBus n = Just (read n)

input :: IO Notes
input = readNotes <$> readFile "data/Day13.txt"

-- Testing

prop_regression :: Property
prop_regression =
  ioProperty $
    ( \n ->
        (uncurry (*) . nextBus) n == 4722
          && (solve . constraints) n == 825305207525452
    )
      <$> input

testNotes :: Notes
testNotes =
  readNotes
    "939\n\
    \7,13,x,x,59,x,31,19"

prop_nextBusUnit :: Property
prop_nextBusUnit =
  property $
    nextBus testNotes == (59, 5)
      && nextBus (Notes 2 [Just 2, Just 3, Just 4]) == (2, 0)

prop_timeTillNextUnit :: Property
prop_timeTillNextUnit =
  property $
    timeTillNext testNotes 59 == 5

prop_solveUnit :: Property
prop_solveUnit =
  property $
    solve (constraints testNotes) == 1068781

return []

runTests :: IO Bool
runTests = $quickCheckAll