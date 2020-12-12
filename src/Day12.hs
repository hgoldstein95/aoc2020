{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day12 where

import Control.Lens (Lens', makeLenses, over, (^.), _1, _2)
import Data.Monoid (Endo (Endo, appEndo))
import Test.QuickCheck (Property, ioProperty, property)
import Test.QuickCheck.All (quickCheckAll)

data Cardinal = N | S | E | W
  deriving (Show, Eq)

data Direction = L | R
  deriving (Show, Eq)

data Instr = Move Cardinal Int | Turn Direction Int | Forward Int
  deriving (Show, Eq)

data Ferry = Ferry {_ferryPos :: (Int, Int), _waypointDelta :: (Int, Int)}
  deriving (Show, Eq)

makeLenses ''Ferry

manhattan :: Ferry -> Int
manhattan f = abs (f ^. ferryPos . _1) + abs (f ^. ferryPos . _2)

type Delta = Ferry -> Ferry

move :: Lens' Ferry (Int, Int) -> Cardinal -> Int -> Delta
move l N i = over (l . _2) (+ i)
move l S i = over (l . _2) (+ (- i))
move l E i = over (l . _1) (+ i)
move l W i = over (l . _1) (+ (- i))

forward :: Int -> Delta
forward i (Ferry (fx, fy) w@(wx, wy)) = Ferry (fx + i * wx, fy + i * wy) w

turn :: Direction -> Int -> Delta
turn d i = (!! div i 90) . iterate once
  where
    once (Ferry f (wx, wy)) =
      case d of
        R -> Ferry f (wy, - wx)
        L -> Ferry f (- wy, wx)

eval :: Lens' Ferry (Int, Int) -> Instr -> Delta
eval l (Move c i) = move l c i
eval _ (Turn d i) = turn d i
eval _ (Forward i) = forward i

simulate1 :: [Instr] -> Ferry
simulate1 = (`appEndo` Ferry (0, 0) (1, 0)) . foldMap (Endo . eval ferryPos) . reverse

simulate2 :: [Instr] -> Ferry
simulate2 = (`appEndo` Ferry (0, 0) (10, 1)) . foldMap (Endo . eval waypointDelta) . reverse

part1 :: IO ()
part1 = print . manhattan . simulate1 =<< input

part2 :: IO ()
part2 = print . manhattan . simulate2 =<< input

-- Input handling

readInstr :: String -> Instr
readInstr ('N' : xs) = Move N $ read xs
readInstr ('S' : xs) = Move S $ read xs
readInstr ('E' : xs) = Move E $ read xs
readInstr ('W' : xs) = Move W $ read xs
readInstr ('L' : xs) = Turn L $ read xs
readInstr ('R' : xs) = Turn R $ read xs
readInstr ('F' : xs) = Forward $ read xs
readInstr xs = error $ "cannot parse '" ++ xs ++ "' as instruction"

readInstrs :: String -> [Instr]
readInstrs = map readInstr . lines

input :: IO [Instr]
input = readInstrs <$> readFile "data/Day12.txt"

-- Testing

prop_regression :: Property
prop_regression =
  ioProperty $
    ( \xs ->
        (manhattan . simulate1) xs == 1186
          && (manhattan . simulate2) xs == 47806
    )
      <$> input

testInstrs :: [Instr]
testInstrs =
  readInstrs
    "F10\n\
    \N3\n\
    \F7\n\
    \R90\n\
    \F11"

prop_simulate1Unit :: Property
prop_simulate1Unit =
  property $
    let f = simulate1 testInstrs
     in f == Ferry (17, -8) (0, -1)
          && manhattan f == 25

prop_simulate2Unit :: Property
prop_simulate2Unit =
  property $
    let f = simulate2 testInstrs
     in f == Ferry (214, -72) (4, -10)
          && manhattan f == 286

prop_forwardUnit :: Property
prop_forwardUnit =
  property $
    forward 3 (Ferry (0, 0) (1, 0)) == Ferry (3, 0) (1, 0)
      && forward 2 (Ferry (0, 0) (-5, -1)) == Ferry (-10, -2) (-5, -1)

prop_turnUnit :: Property
prop_turnUnit =
  property $
    turn R 90 (Ferry (0, 0) (3, 2)) == Ferry (0, 0) (2, -3)
      && turn R 180 (Ferry (0, 0) (3, 2)) == Ferry (0, 0) (-3, -2)
      && turn L 90 (Ferry (0, 0) (3, 2)) == Ferry (0, 0) (-2, 3)
      && turn L 180 (Ferry (0, 0) (3, 2)) == Ferry (0, 0) (-3, -2)

prop_moveUnit :: Property
prop_moveUnit =
  property $
    move ferryPos N 4 (Ferry (0, 0) (1, 0)) == Ferry (0, 4) (1, 0)
      && move ferryPos W 3 (Ferry (0, 0) (1, 0)) == Ferry (-3, 0) (1, 0)

return []

runTests :: IO Bool
runTests = $quickCheckAll