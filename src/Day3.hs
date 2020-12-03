{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Day3 where

import Test.QuickCheck (Arbitrary (..), Property, elements, ioProperty, listOf, property, vectorOf, (===))
import Test.QuickCheck.All (quickCheckAll)

type Slope = (Int, Int)

type Square = (Int, Int)

height :: Mountain -> Int
height = length . getSquares

width :: Mountain -> Int
width = length . head . getSquares

index :: Square -> Mountain -> Entity
index (x, y) m = getSquares m !! y !! (x `mod` width m)

walk :: Square -> Slope -> Mountain -> [Entity]
walk (_, y) _ m | y >= height m = []
walk p@(x, y) s@(dx, dy) m = index p m : walk (x + dx, y + dy) s m

countTrees :: Slope -> Mountain -> Int
countTrees s = length . filter (== Tree) . walk (0, 0) s

slopes :: [Slope]
slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

countAndMultiply :: [Slope] -> Mountain -> Int
countAndMultiply s m = product $ map (`countTrees` m) s

part1 :: IO ()
part1 = print . countTrees (3, 1) =<< input

part2 :: IO ()
part2 = print . countAndMultiply slopes =<< input

-- Input handling

data Entity = Tree | Empty
  deriving (Eq)

newtype Mountain = Mountain {getSquares :: [[Entity]]}
  deriving (Eq)

instance Show Mountain where
  show = unlines . map (map showSquare) . getSquares
    where
      showSquare Tree = '#'
      showSquare Empty = '.'

instance Read Mountain where
  readsPrec _ = (\x -> [(x, "")]) . Mountain . map (map parseSquare) . lines
    where
      parseSquare '#' = Tree
      parseSquare '.' = Empty
      parseSquare _ = error "invalid symbol"

input :: IO Mountain
input = read <$> readFile "data/Day3.txt"

-- Testing

instance Arbitrary Entity where
  arbitrary = elements [Tree, Empty]

instance Arbitrary Mountain where
  arbitrary = do
    x <- elements [5 .. 20]
    Mountain <$> listOf (vectorOf x arbitrary)

prop_regression :: Property
prop_regression =
  ioProperty $
    (\m -> countTrees (3, 1) m == 268 && countAndMultiply slopes m == 3093068400) <$> input

prop_roundTrip :: Mountain -> Property
prop_roundTrip ps = ps === read (show ps)

testMountain :: Mountain
testMountain =
  read
    "..##.......\n\
    \#...#...#..\n\
    \.#....#..#.\n\
    \..#.#...#.#\n\
    \.#...##..#.\n\
    \..#.##.....\n\
    \.#.#.#....#\n\
    \.#........#\n\
    \#.##...#...\n\
    \#...##....#\n\
    \.#..#...#.#"

prop_countTreesUnit :: Property
prop_countTreesUnit = property $ countTrees (3, 1) testMountain == 7

prop_countAndMultiplyUnit :: Property
prop_countAndMultiplyUnit = property $ countAndMultiply slopes testMountain == 336

return []

runTests :: IO Bool
runTests = $quickCheckAll