{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Day24 where

import Data.Functor (($>))
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Test.QuickCheck (Property, ioProperty, property)
import Test.QuickCheck.All (quickCheckAll)
import Text.Parser (parse, (<++))
import qualified Text.Parser as P

type Pos = (Int, Int)

type BlackTiles = Set Pos

data Dir = E | NE | SE | W | NW | SW deriving (Eq, Show)

move :: Dir -> Pos -> Pos
move E (x, y) = (x + 1, y)
move W (x, y) = (x - 1, y)
move NE (x, y) = (x + 1, y + 1)
move NW (x, y) = (x, y + 1)
move SW (x, y) = (x - 1, y - 1)
move SE (x, y) = (x, y - 1)

neighbors :: Pos -> Set Pos
neighbors (x, y) =
  Set.fromList
    [ (x + 1, y),
      (x - 1, y),
      (x + 1, y + 1),
      (x, y + 1),
      (x - 1, y - 1),
      (x, y - 1)
    ]

flipTile :: [Dir] -> BlackTiles -> BlackTiles
flipTile ds s =
  let p = foldl' (flip move) (0, 0) ds
   in if p `Set.member` s then Set.delete p s else Set.insert p s

setInitial :: [[Dir]] -> BlackTiles
setInitial = foldl' (flip flipTile) Set.empty

stepDay :: BlackTiles -> BlackTiles
stepDay b =
  let candidates = b `Set.union` Set.unions (Set.map neighbors b)
   in Set.filter stepsToBlack candidates
  where
    isBlack = (`Set.member` b)
    count p = Set.size . Set.filter p
    stepsToBlack p
      | isBlack p = count isBlack (neighbors p) `elem` [1, 2]
      | otherwise = count isBlack (neighbors p) == 2

simulate :: Int -> BlackTiles -> BlackTiles
simulate n = (!! n) . iterate stepDay

part1 :: IO ()
part1 = print . Set.size . setInitial =<< input

part2 :: IO ()
part2 = print . Set.size . simulate 100 . setInitial =<< input

-- Input handling

readDirs :: String -> [[Dir]]
readDirs = map (fromJust . parse dirList) . lines
  where
    dirList = P.many1 dir
    dir = ne <++ nw <++ se <++ sw <++ e <++ w
    ne = P.string "ne" $> NE
    nw = P.string "nw" $> NW
    se = P.string "se" $> SE
    sw = P.string "sw" $> SW
    e = P.char 'e' $> E
    w = P.char 'w' $> W

input :: IO [[Dir]]
input = readDirs <$> readFile "data/Day24.txt"

-- Testing

prop_regression :: Property
prop_regression =
  ioProperty $
    ( \ds ->
        (Set.size . setInitial) ds == 282
          && (Set.size . simulate 100 . setInitial) ds == 3445
    )
      <$> input

testDirs :: [[Dir]]
testDirs =
  readDirs
    "sesenwnenenewseeswwswswwnenewsewsw\n\
    \neeenesenwnwwswnenewnwwsewnenwseswesw\n\
    \seswneswswsenwwnwse\n\
    \nwnwneseeswswnenewneswwnewseswneseene\n\
    \swweswneswnenwsewnwneneseenw\n\
    \eesenwseswswnenwswnwnwsewwnwsene\n\
    \sewnenenenesenwsewnenwwwse\n\
    \wenwwweseeeweswwwnwwe\n\
    \wsweesenenewnwwnwsenewsenwwsesesenwne\n\
    \neeswseenwwswnwswswnw\n\
    \nenwswwsewswnenenewsenwsenwnesesenew\n\
    \enewnwewneswsewnwswenweswnenwsenwsw\n\
    \sweneswneswneneenwnewenewwneswswnese\n\
    \swwesenesewenwneswnwwneseswwne\n\
    \enesenwswwswneneswsenwnewswseenwsese\n\
    \wnwnesenesenenwwnenwsewesewsesesew\n\
    \nenewswnwewswnenesenwnesewesw\n\
    \eneswnwswnwsenenwnwnwwseeswneewsenese\n\
    \neswnwewnwnwseenwseesewsenwsweewe\n\
    \wseweeenwnesenwwwswnew"

prop_setInitialUnit :: Property
prop_setInitialUnit = property $ (Set.size . setInitial) testDirs == 10

prop_simulateUnit :: Property
prop_simulateUnit = property $ (Set.size . simulate 100 . setInitial) testDirs == 2208

return []

runTests :: IO Bool
runTests = $quickCheckAll