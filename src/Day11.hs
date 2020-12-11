{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Day11 where

import Control.Arrow (Arrow ((***)))
import Data.Array (Array, Ix, array, bounds, elems, inRange, indices, (!), (//))
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Test.QuickCheck
  ( Arbitrary (..),
    Property,
    elements,
    getPositive,
    ioProperty,
    property,
    (===),
  )
import Test.QuickCheck.All (quickCheckAll)

imap :: (Ix i) => (i -> a -> a) -> Array i a -> Array i a
imap f a = a // [(i, f i (a ! i)) | i <- indices a]

deltas :: [(Int, Int) -> (Int, Int)]
deltas = [(+ dx) *** (+ dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

neighbors :: (Int, Int) -> Array (Int, Int) a -> [a]
neighbors p a = mapMaybe ((a !?) . ($ p)) deltas

arrayToList :: Array (Int, Int) a -> [[a]]
arrayToList a =
  let (_, (mx, my)) = bounds a
   in [[a ! (x, y) | x <- [0 .. mx]] | y <- [0 .. my]]

listToArray :: [[a]] -> Array (Int, Int) a
listToArray xs =
  let mx = length (head xs) - 1
      my = length xs - 1
   in array ((0, 0), (mx, my)) [((x, y), xs !! y !! x) | x <- [0 .. mx], y <- [0 .. my]]

(!?) :: (Ix i) => Array i a -> i -> Maybe a
a !? i = if inRange (bounds a) i then Just (a ! i) else Nothing

fp :: Eq a => (a -> a) -> a -> a
fp f x =
  let x' = f x
   in if x == x' then x' else fp f x'

data Space = Floor | Seat | Person
  deriving (Eq, Show)

newtype Layout = Layout {getSpaces :: Array (Int, Int) Space}
  deriving (Eq, Show)

occupied :: Space -> Bool
occupied Person = True
occupied _ = False

occupiedAfterStable :: (Layout -> Layout) -> Layout -> Int
occupiedAfterStable s = length . filter occupied . elems . getSpaces . fp s

step1 :: Layout -> Layout
step1 (Layout spaces) = Layout $ imap stepSpace spaces
  where
    occupiedNeighbors i = length (filter occupied (neighbors i spaces))
    stepSpace _ Floor = Floor
    stepSpace i Seat = if occupiedNeighbors i == 0 then Person else Seat
    stepSpace i Person = if occupiedNeighbors i >= 4 then Seat else Person

visibleChairs :: (Int, Int) -> Array (Int, Int) Space -> [Space]
visibleChairs pt a = mapMaybe (\d -> visibleChair d (d pt)) deltas
  where
    visibleChair f p
      | not (inRange (bounds a) p) = Nothing
      | a ! p /= Floor = Just (a ! p)
      | otherwise = visibleChair f (f p)

step2 :: Layout -> Layout
step2 (Layout spaces) = Layout $ imap stepSpace spaces
  where
    occupiedVisible i = length (filter occupied (visibleChairs i spaces))
    stepSpace _ Floor = Floor
    stepSpace i Seat = if occupiedVisible i == 0 then Person else Seat
    stepSpace i Person = if occupiedVisible i >= 5 then Seat else Person

part1 :: IO ()
part1 = print . occupiedAfterStable step1 =<< input

part2 :: IO ()
part2 = print . occupiedAfterStable step2 =<< input

-- Layout handling

class Pretty a where
  pretty :: a -> String

instance Pretty Layout where
  pretty (Layout spaces) =
    intercalate "\n"
      . map (map spaceChar)
      . arrayToList
      $ spaces
    where
      spaceChar Floor = '.'
      spaceChar Seat = 'L'
      spaceChar Person = '#'

instance Read Layout where
  readsPrec _ = (\x -> [(x, "")]) . Layout . listToArray . map (map charSpace) . lines
    where
      charSpace '.' = Floor
      charSpace 'L' = Seat
      charSpace '#' = Person
      charSpace _ = error "bad parse"

input :: IO Layout
input = read <$> readFile "data/Day11.txt"

-- Testing

instance Arbitrary Space where
  arbitrary = elements [Floor, Seat, Person]

iterateM :: Monad m => m a -> m [a]
iterateM m = (:) <$> m <*> iterateM m

instance Arbitrary Layout where
  arbitrary = do
    n <- getPositive <$> arbitrary
    Layout
      . array ((0, 0), (n, n))
      . zip [(a, b) | a <- [0 .. n], b <- [0 .. n]]
      <$> iterateM arbitrary

prop_regression :: Property
prop_regression =
  ioProperty $
    ( \l ->
        occupiedAfterStable step1 l == 2494
          && occupiedAfterStable step2 l == 2306
    )
      <$> input

prop_roundTrip :: Layout -> Property
prop_roundTrip ps = ps === read (pretty ps)

testLayout :: Layout
testLayout =
  read
    "L.LL.LL.LL\n\
    \LLLLLLL.LL\n\
    \L.L.L..L..\n\
    \LLLL.LL.LL\n\
    \L.LL.LL.LL\n\
    \L.LLLLL.LL\n\
    \..L.L.....\n\
    \LLLLLLLLLL\n\
    \L.LLLLLL.L\n\
    \L.LLLLL.LL"

prop_occupiedAfterStableUnit :: Property
prop_occupiedAfterStableUnit =
  property $
    occupiedAfterStable step1 testLayout == 37 && occupiedAfterStable step2 testLayout == 26

return []

runTests :: IO Bool
runTests = $quickCheckAll