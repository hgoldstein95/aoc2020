{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Day20 where

import Control.Arrow (first, second, (***))
import Control.Monad (msum)
import Data.List (foldl1', groupBy, intercalate, sortOn, transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Test.QuickCheck (Property, ioProperty, property)
import Test.QuickCheck.All (quickCheckAll)
import Text.Parser (parse, (+++))
import qualified Text.Parser as P
import Prelude hiding (flip)

type Space = Char

type Grid = [[Space]]

type Row = [Space]

data Tile = Tile
  { tileId :: Integer,
    pixels :: Grid
  }
  deriving (Eq, Show)

north, east, south, west :: Grid -> Row
north = head
east = last . transpose
south = last
west = head . transpose

rotate, flip :: Grid -> Grid
rotate = map reverse . transpose
flip = map reverse

orientations :: Grid -> [Grid]
orientations p =
  [ p,
    rotate p,
    (rotate . rotate) p,
    (rotate . rotate . rotate) p,
    flip p,
    (flip . rotate) p,
    (flip . rotate . rotate) p,
    (flip . rotate . rotate . rotate) p
  ]

type Pos = (Int, Int)

data Match = N | E | S | W deriving (Eq)

match :: Grid -> Grid -> Maybe Match
match a b
  | west a == east b = Just W
  | east a == west b = Just E
  | north a == south b = Just N
  | south a == north b = Just S
  | otherwise = Nothing

dir :: Match -> Pos -> Pos
dir W = first (+ (-1))
dir E = first (+ 1)
dir N = second (+ 1)
dir S = second (+ (-1))

matchAny :: Grid -> Grid -> Maybe (Match, Grid)
matchAny a b = msum $ (\b' -> (,b') <$> match a b') <$> orientations b

type Puzzle = Map Pos Tile

puzzle :: [Tile] -> Puzzle
puzzle [] = Map.empty
puzzle (a : as) = fromJust $ aux (Map.singleton (0, 0) a) as
  where
    attach (pos, t1) t2 =
      ((`dir` pos) *** Tile (tileId t2)) <$> matchAny (pixels t1) (pixels t2)

    aux m [] = pure m
    aux m ts = do
      (p, t) <- msum $ attach <$> Map.assocs m <*> ts
      aux (Map.insert p t m) . filter ((/= tileId t) . tileId) $ ts

valid :: Pos -> Tile -> Puzzle -> Bool
valid p t m =
  all
    ( \d ->
        case m Map.!? dir d p of
          Nothing -> True
          Just t' -> match (pixels t) (pixels t') == Just d
    )
    [N, E, S, W]

corners :: Puzzle -> [Tile]
corners m =
  [ m Map.! (x, y)
    | x <- [minimum, maximum] <*> [fst <$> Map.keys m],
      y <- [minimum, maximum] <*> [snd <$> Map.keys m]
  ]

crop :: Tile -> Tile
crop (Tile pid cs) = Tile pid $ (transpose . init . tail . transpose . init . tail) cs

stitch :: Map Pos Tile -> Grid
stitch =
  concat
    . reverse
    . map (foldl1' (zipWith (++)) . map (pixels . snd))
    . groupBy (\a b -> (snd . fst) a == (snd . fst) b)
    . sortOn (snd . fst)
    . map (second crop)
    . Map.assocs

countMonsters :: Grid -> Int
countMonsters =
  length
    . msum
    . map
      ( (\m -> filter (`isMonster` m) (Map.keys m))
          . withPositions
      )
    . orientations
  where
    withPositions grid =
      Map.fromList
        [ ((row, col), value)
          | (row, content) <- zip [0 :: Int ..] grid,
            (col, value) <- zip [0 :: Int ..] content
        ]

    isMonster (x, y) m =
      all
        ((== Just '#') . (m Map.!?))
        [ (x + 18, y),
          (x, y + 1),
          (x + 5, y + 1),
          (x + 6, y + 1),
          (x + 11, y + 1),
          (x + 12, y + 1),
          (x + 17, y + 1),
          (x + 18, y + 1),
          (x + 19, y + 1),
          (x + 1, y + 2),
          (x + 4, y + 2),
          (x + 7, y + 2),
          (x + 10, y + 2),
          (x + 13, y + 2),
          (x + 16, y + 2)
        ]

checkWaters :: Puzzle -> Int
checkWaters (stitch -> g) =
  (length . filter (== '#') . concat) g - ((* 15) . countMonsters) g

showGrid :: Grid -> String
showGrid = intercalate "\n"

part1 :: IO ()
part1 = print . product . map tileId . corners . puzzle =<< input

part2 :: IO ()
part2 = print . checkWaters . puzzle =<< input

-- Input handling

readTiles :: String -> [Tile]
readTiles = fromJust . parse tiles
  where
    tiles = tile `P.sepBy` P.char '\n'
    tile = P.string "Tile " *> (Tile <$> (toInteger <$> P.nat) <* P.string ":\n" <*> ppixels)
    ppixels = P.count 10 row
    row = P.count 10 pixel <* P.char '\n'
    pixel = P.char '#' +++ P.char '.'

input :: IO [Tile]
input = readTiles <$> readFile "data/Day20.txt"

-- Testing

prop_regression :: Property
prop_regression =
  ioProperty $
    ( \ts ->
        let p = puzzle ts
         in (product . map tileId . corners) p == 32287787075651
              && checkWaters p == 1939
    )
      <$> input

testTiles :: [Tile]
testTiles =
  readTiles
    "Tile 2311:\n\
    \..##.#..#.\n\
    \##..#.....\n\
    \#...##..#.\n\
    \####.#...#\n\
    \##.##.###.\n\
    \##...#.###\n\
    \.#.#.#..##\n\
    \..#....#..\n\
    \###...#.#.\n\
    \..###..###\n\
    \\n\
    \Tile 1951:\n\
    \#.##...##.\n\
    \#.####...#\n\
    \.....#..##\n\
    \#...######\n\
    \.##.#....#\n\
    \.###.#####\n\
    \###.##.##.\n\
    \.###....#.\n\
    \..#.#..#.#\n\
    \#...##.#..\n\
    \\n\
    \Tile 1171:\n\
    \####...##.\n\
    \#..##.#..#\n\
    \##.#..#.#.\n\
    \.###.####.\n\
    \..###.####\n\
    \.##....##.\n\
    \.#...####.\n\
    \#.##.####.\n\
    \####..#...\n\
    \.....##...\n\
    \\n\
    \Tile 1427:\n\
    \###.##.#..\n\
    \.#..#.##..\n\
    \.#.##.#..#\n\
    \#.#.#.##.#\n\
    \....#...##\n\
    \...##..##.\n\
    \...#.#####\n\
    \.#.####.#.\n\
    \..#..###.#\n\
    \..##.#..#.\n\
    \\n\
    \Tile 1489:\n\
    \##.#.#....\n\
    \..##...#..\n\
    \.##..##...\n\
    \..#...#...\n\
    \#####...#.\n\
    \#..#.#.#.#\n\
    \...#.#.#..\n\
    \##.#...##.\n\
    \..##.##.##\n\
    \###.##.#..\n\
    \\n\
    \Tile 2473:\n\
    \#....####.\n\
    \#..#.##...\n\
    \#.##..#...\n\
    \######.#.#\n\
    \.#...#.#.#\n\
    \.#########\n\
    \.###.#..#.\n\
    \########.#\n\
    \##...##.#.\n\
    \..###.#.#.\n\
    \\n\
    \Tile 2971:\n\
    \..#.#....#\n\
    \#...###...\n\
    \#.#.###...\n\
    \##.##..#..\n\
    \.#####..##\n\
    \.#..####.#\n\
    \#..#.#..#.\n\
    \..####.###\n\
    \..#.#.###.\n\
    \...#.#.#.#\n\
    \\n\
    \Tile 2729:\n\
    \...#.#.#.#\n\
    \####.#....\n\
    \..#.#.....\n\
    \....#..#.#\n\
    \.##..##.#.\n\
    \.#.####...\n\
    \####.#.#..\n\
    \##.####...\n\
    \##..#.##..\n\
    \#.##...##.\n\
    \\n\
    \Tile 3079:\n\
    \#.#.#####.\n\
    \.#..######\n\
    \..#.......\n\
    \######....\n\
    \####.#..#.\n\
    \.#...#.##.\n\
    \#.#####.##\n\
    \..#.###...\n\
    \..#.......\n\
    \..#.###...\n"

prop_cornersUnit :: Property
prop_cornersUnit =
  property $ (product . map tileId . corners . puzzle) testTiles == 20899048083289

prop_stitchUnit :: Property
prop_stitchUnit =
  property $
    elem g
      . orientations
      . stitch
      . puzzle
      $ testTiles
  where
    g =
      [ ".#.#..#.##...#.##..#####",
        "###....#.#....#..#......",
        "##.##.###.#.#..######...",
        "###.#####...#.#####.#..#",
        "##.#....#.##.####...#.##",
        "...########.#....#####.#",
        "....#..#...##..#.#.###..",
        ".####...#..#.....#......",
        "#..#.##..#..###.#.##....",
        "#.####..#.####.#.#.###..",
        "###.#.#...#.######.#..##",
        "#.####....##..########.#",
        "##..##.#...#...#.#.#.#..",
        "...#..#..#.#.##..###.###",
        ".#.#....#.##.#...###.##.",
        "###.#...#..#.##.######..",
        ".#.#.###.##.##.#..#.##..",
        ".####.###.#...###.#..#.#",
        "..#.#..#..#.#.#.####.###",
        "#..####...#.#.#.###.###.",
        "#####..#####...###....##",
        "#.##..#..#...#..####...#",
        ".#.###..##..##..####.##.",
        "...###...##...#...#..###"
      ]

prop_checkWatersUnit :: Property
prop_checkWatersUnit = property $ (checkWaters . puzzle) testTiles == 273

return []

runTests :: IO Bool
runTests = $quickCheckAll
