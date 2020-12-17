{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day17 where

import Control.Lens (makeLenses, view)
import Data.Set (Set)
import qualified Data.Set as Set
import Test.QuickCheck (Property, ioProperty, property)
import Test.QuickCheck.All (quickCheckAll)

{-# ANN module "HLint: ignore Reduce duplication" #-}

type Point3D = (Int, Int, Int)

type Point4D = (Int, Int, Int, Int)

class Ord p => Point p where
  mapPoint :: (Int -> Int) -> p -> p
  mapPoint2 :: (Int -> Int -> Int) -> p -> p -> p
  range :: p -> p -> Set p
  fromXY :: (Int, Int) -> p

instance Point Point3D where
  mapPoint f (x, y, z) = (f x, f y, f z)
  mapPoint2 f (x1, y1, z1) (x2, y2, z2) = (f x1 x2, f y1 y2, f z1 z2)
  range (x1, y1, z1) (x2, y2, z2) =
    Set.fromDistinctAscList
      [ (x, y, z)
        | x <- [x1 .. x2],
          y <- [y1 .. y2],
          z <- [z1 .. z2]
      ]
  fromXY (x, y) = (x, y, 0)

instance Point Point4D where
  mapPoint f (x, y, z, w) = (f x, f y, f z, f w)
  mapPoint2 f (x1, y1, z1, w1) (x2, y2, z2, w2) = (f x1 x2, f y1 y2, f z1 z2, f w1 w2)
  range (x1, y1, z1, w1) (x2, y2, z2, w2) =
    Set.fromDistinctAscList
      [ (x, y, z, w)
        | x <- [x1 .. x2],
          y <- [y1 .. y2],
          z <- [z1 .. z2],
          w <- [w1 .. w2]
      ]
  fromXY (x, y) = (x, y, 0, 0)

neighbors :: Point p => p -> Set p
neighbors p = Set.delete p $ range (mapPoint (+ (-1)) p) (mapPoint (+ 1) p)

type Board p = Set p

active :: Ord p => Board p -> p -> Bool
active b p = p `Set.member` b

newtype Game p = Game {_board :: Board p}
  deriving (Show)

makeLenses ''Game

stepBoard :: Point p => Set p -> Board p -> Board p
stepBoard ps b = Set.filter keep ps
  where
    activeNeighbors p = Set.size $ Set.filter (active b) (neighbors p)
    keep p
      | active b p = activeNeighbors p `elem` [2, 3]
      | otherwise = activeNeighbors p == 3

step :: Point p => Game p -> Game p
step (Game b) =
  let minB' = mapPoint (+ (- 1)) $ foldl1 (mapPoint2 min) b
      maxB' = mapPoint (+ 1) $ foldl1 (mapPoint2 max) b
   in Game (stepBoard (range minB' maxB') b)

countActive :: Game p -> Int
countActive = Set.size . view board

part1 :: IO ()
part1 = print . countActive . (!! 6) . iterate step =<< (input :: IO (Game Point3D))

part2 :: IO ()
part2 = print . countActive . (!! 6) . iterate step =<< (input :: IO (Game Point4D))

-- Input handling

readGame :: Point p => String -> Game p
readGame s =
  let grid = lines s
   in Game (activePoints grid)
  where
    activePoints ccs =
      Set.fromList
        [ fromXY (x, y)
          | (y, cs) <- zip [0 ..] ccs,
            (x, c) <- zip [0 ..] cs,
            c == '#'
        ]

input :: Point p => IO (Game p)
input = readGame <$> readFile "data/Day17.txt"

-- Testing

prop_regression1 :: Property
prop_regression1 =
  ioProperty $ (\g -> (countActive . (!! 6) . iterate step) (g :: Game Point3D) == 336) <$> input

prop_regression2 :: Property
prop_regression2 =
  ioProperty $ (\g -> (countActive . (!! 6) . iterate step) (g :: Game Point4D) == 2620) <$> input

testGame :: Point p => Game p
testGame =
  readGame
    ".#.\n\
    \..#\n\
    \###"

prop_stepCountActiveUnit :: Property
prop_stepCountActiveUnit =
  property $
    (countActive . (!! 6) . iterate step) (testGame :: Game Point3D) == 112
      && (countActive . (!! 6) . iterate step) (testGame :: Game Point4D) == 848

return []

runTests :: IO Bool
runTests = $quickCheckAll