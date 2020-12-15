{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Day15 where

import Control.Lens (makeLenses, view, (^.))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.List (unfoldr)
import Data.List.Split (splitOn)
import Test.QuickCheck (Property, ioProperty, property)
import Test.QuickCheck.All (quickCheckAll)

data Game = Game {_lastSeen :: !(IntMap Int), _currentStep :: !Int, _currentVal :: !Int}
  deriving (Show, Eq, Ord)

makeLenses ''Game

makeGame :: [Int] -> Game
makeGame xs =
  let (ys, [y]) = splitAt (length xs - 1) xs
   in Game (Map.fromList $ zip ys [1 ..]) (length xs) y

step :: Game -> Game
step (Game m s v) =
  Game
    (Map.insert v s m)
    (s + 1)
    (maybe 0 (s -) $ m Map.!? v)

stepTo :: Int -> Game -> Game
stepTo i = last . unfoldr (\g -> if g ^. currentStep == i + 1 then Nothing else Just (g, step g))

part1 :: IO ()
part1 = print . view currentVal . stepTo 2020 =<< input

part2 :: IO ()
part2 = print . view currentVal . stepTo 30000000 =<< input

-- Input handling

readGame :: String -> Game
readGame = makeGame . map read . splitOn ","

input :: IO Game
input = readGame <$> readFile "data/Day15.txt"

-- Testing

prop_regression :: Property
prop_regression =
  ioProperty $
    ( \g ->
        stepTo 2020 g ^. currentVal == 421
          && stepTo 30000000 g ^. currentVal == 436 -- Warning, takes a while
    )
      <$> input

prop_stepToUnit :: Property
prop_stepToUnit = property $ stepTo 2020 (makeGame [0, 3, 6]) ^. currentVal == 436

return []

runTests :: IO Bool
runTests = $quickCheckAll