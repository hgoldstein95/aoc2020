{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Day11 where

import Control.Arrow (Arrow ((***)))
import Control.Monad (replicateM)
import Data.List (intercalate, sort)
import Data.Maybe (mapMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Test.QuickCheck
  ( Arbitrary (..),
    Positive (..),
    Property,
    elements,
    getPositive,
    ioProperty,
    property,
    (===),
  )
import Test.QuickCheck.All (quickCheckAll)

type Point = (Int, Int)

data Space = Floor | Seat | Person
  deriving (Eq, Show)

data Layout = Layout {getSpaces :: !(Vector Space), upperBound :: !Point}
  deriving (Eq, Show)

imap :: (Point -> Space -> Space) -> Layout -> Layout
imap f (Layout v m) = Layout (Vector.imap (f . indexToPoint m) v) m

pointToIndex :: Point -> Point -> Int
pointToIndex (mx, _) (x, y) = y * (mx + 1) + x

indexToPoint :: Point -> Int -> Point
indexToPoint (mx, _) i = (i `mod` (mx + 1), i `div` (mx + 1))

layoutToList :: Layout -> [[Space]]
layoutToList l@(Layout _ (mx, my)) =
  [[l ! (x, y) | x <- [0 .. mx]] | y <- [0 .. my]]

listToLayout :: [[Space]] -> Layout
listToLayout xs =
  let mx = length (head xs) - 1
      my = length xs - 1
   in Layout (Vector.fromList . concat $ xs) (mx, my)

(!) :: Layout -> Point -> Space
(!) (Layout v m) p = v Vector.! pointToIndex m p

(!?) :: Layout -> Point -> Maybe Space
l !? p = if not (inBounds l p) then Nothing else Just (l ! p)

inBounds :: Layout -> Point -> Bool
inBounds (Layout _ (mx, my)) (px, py) = 0 <= px && px <= mx && 0 <= py && py <= my

spaces :: Layout -> [Space]
spaces = Vector.toList . getSpaces

-- Main Code

fp :: Eq a => (a -> a) -> a -> a
fp f x =
  let x' = f x
   in if x == x' then x' else fp f x'

deltas :: [Point -> Point]
deltas = [(+ dx) *** (+ dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

occupied :: Space -> Bool
occupied Person = True
occupied _ = False

occupiedAfterStable :: (Layout -> Layout) -> Layout -> Int
occupiedAfterStable s = length . filter occupied . spaces . fp s

neighbors :: Point -> Layout -> [Space]
neighbors p a = mapMaybe ((a !?) . ($ p)) deltas

step1 :: Layout -> Layout
step1 l = imap stepSpace l
  where
    occupiedNeighbors i = length (filter occupied (neighbors i l))
    stepSpace _ Floor = Floor
    stepSpace i Seat = if occupiedNeighbors i == 0 then Person else Seat
    stepSpace i Person = if occupiedNeighbors i >= 4 then Seat else Person

visibleChairs :: Point -> Layout -> [Space]
visibleChairs pt l = mapMaybe (\d -> visibleChair d (d pt)) deltas
  where
    visibleChair f p
      | not (inBounds l p) = Nothing
      | l ! p /= Floor = Just (l ! p)
      | otherwise = visibleChair f (f p)

step2 :: Layout -> Layout
step2 l = imap stepSpace l
  where
    occupiedVisible i = length (filter occupied (visibleChairs i l))
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
  pretty = intercalate "\n" . map (map spaceChar) . layoutToList
    where
      spaceChar Floor = '.'
      spaceChar Seat = 'L'
      spaceChar Person = '#'

instance Read Layout where
  readsPrec _ = (\x -> [(x, "")]) . listToLayout . map (map charSpace) . lines
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
    listToLayout <$> replicateM n (replicateM n arbitrary)

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

prop_indexPointRT ::
  Positive Int ->
  Positive Int ->
  Positive Int ->
  Positive Int ->
  Property
prop_indexPointRT (Positive a) (Positive b) (Positive c) (Positive d) =
  let [x, mx] = sort [a, b]
      [y, my] = sort [c, d]
   in (indexToPoint (mx, my) . pointToIndex (mx, my)) (x, y) === (x, y)

return []

runTests :: IO Bool
runTests = $quickCheckAll