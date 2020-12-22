{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day20 where

import Control.Arrow (first, second, (***))
import Control.Monad (msum)
import Data.List (foldl1', groupBy, intercalate, sortOn, transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, maybeToList)
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
    (rotate . flip) p,
    (rotate . rotate . flip) p,
    (rotate . rotate . rotate . flip) p
  ]

type Pos = (Int, Int)

data Match = N | E | S | W deriving (Eq)

match :: Grid -> Grid -> Maybe Match
match a b
  | east a == west b = Just E
  | west a == east b = Just W
  | north a == south b = Just N
  | south a == north b = Just S
  | otherwise = Nothing

dir :: Match -> Pos -> Pos
dir E = first (+ 1)
dir W = first (+ (-1))
dir N = second (+ 1)
dir S = second (+ (-1))

matchAny :: Grid -> Grid -> Maybe (Match, Grid)
matchAny a b = msum $ (\b' -> (,b') <$> match a b') <$> orientations b

type Puzzle = Map Pos Tile

puzzle :: [Tile] -> Puzzle
puzzle [] = Map.empty
puzzle (a : as) = head $ aux (Map.singleton (0, 0) a) as
  where
    attach (pos, t1) t2 =
      ((`dir` pos) *** Tile (tileId t2)) <$> matchAny (pixels t1) (pixels t2)

    aux m [] = pure m
    aux m ts = do
      pt <- Map.assocs m
      t <- ts
      (p', t') <- maybeToList (attach pt t)
      aux (Map.insert p' t' m) . filter ((/= tileId t') . tileId) $ ts

corners :: Puzzle -> [Tile]
corners m =
  [ m Map.! (x, y)
    | x <- [minimum, maximum] <*> [fst <$> Map.keys m],
      y <- [minimum, maximum] <*> [snd <$> Map.keys m]
  ]

crop :: Tile -> Tile
crop (Tile pid cs) = Tile pid $ (transpose . init . tail . transpose . init) cs

stitch :: Map Pos Tile -> Grid
stitch =
  concat
    . reverse
    . map (foldl1' (zipWith (++)) . map (pixels . snd))
    . groupBy (\a b -> (snd . fst) a == (snd . fst) b)
    . sortOn (snd . fst)
    . map (second crop)
    . Map.assocs

showGrid :: Grid -> String
showGrid = intercalate "\n"

part1 :: IO ()
part1 = print . product . map tileId . corners . puzzle =<< input

part2 :: IO ()
part2 = undefined

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

return []

runTests :: IO Bool
runTests = $quickCheckAll