{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Day7 where

import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.QuickCheck (Property, ioProperty, property)
import Test.QuickCheck.All (quickCheckAll)
import Text.Parsec
  ( anyChar,
    char,
    digit,
    endOfLine,
    many,
    many1,
    manyTill,
    optional,
    parse,
    sepBy,
    space,
    string,
    (<|>),
  )

type Bag = String

newtype Contains = Contains {getContainments :: Map Bag [(Int, Bag)]}
  deriving (Eq, Show)

fixSet :: Ord a => (a -> Set a) -> Set a -> Set a
fixSet f a =
  let a' = (Set.unions . Set.map f) a
   in if a == a' then a' else a `Set.union` fixSet f a'

containing :: Contains -> Bag -> Set Bag
containing c b =
  Set.fromList
    . map fst
    . filter (any ((== b) . snd) . snd)
    . Map.toList
    . getContainments
    $ c

transContaining :: Bag -> Contains -> Set Bag
transContaining b c = fixSet (containing c) (Set.singleton b) Set.\\ Set.singleton b

countInside :: Bag -> Contains -> Int
countInside b c = sum . map (\(i, b') -> i * (1 + countInside b' c)) $ getContainments c Map.! b

part1 :: IO ()
part1 = print . Set.size . transContaining "shiny gold" =<< input

part2 :: IO ()
part2 = print . countInside "shiny gold" =<< input

-- Input handling

instance Read Contains where
  readsPrec _ s = [(either (error . show) id . parse parseInput "" $ s, "")]
    where
      parseInput = Contains . Map.fromList <$> many sentence
      sentence = (,) <$> bag <* string " contain " <*> countedBags <* char '.' <* endOfLine
      bag = (\x y -> x ++ " " ++ y) <$> word <*> word <* bagOrBags
      bagOrBags = string "bag" *> optional (char 's')
      countedBags = (string "no other bags" $> []) <|> (countedBag `sepBy` string ", ")
      countedBag = (,) <$> int <* space <*> bag
      word = manyTill anyChar space
      int = read <$> many1 digit

input :: IO Contains
input = read <$> readFile "data/Day7.txt"

-- Testing

prop_regression :: Property
prop_regression =
  ioProperty $
    ( \c ->
        (Set.size . transContaining "shiny gold") c == 164
          && countInside "shiny gold" c == 7872
    )
      <$> input

testContains1 :: Contains
testContains1 =
  read
    "light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
    \dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
    \bright white bags contain 1 shiny gold bag.\n\
    \muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
    \shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
    \dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
    \vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
    \faded blue bags contain no other bags.\n\
    \dotted black bags contain no other bags.\n"

prop_containingUnit :: Property
prop_containingUnit =
  property $
    containing testContains1 "faded blue"
      == Set.fromList ["dark olive", "vibrant plum", "muted yellow"]

prop_transContainingUnit :: Property
prop_transContainingUnit =
  property $
    transContaining "shiny gold" testContains1
      == Set.fromList ["bright white", "muted yellow", "dark orange", "light red"]

testContains2 :: Contains
testContains2 =
  read
    "shiny gold bags contain 2 dark red bags.\n\
    \dark red bags contain 2 dark orange bags.\n\
    \dark orange bags contain 2 dark yellow bags.\n\
    \dark yellow bags contain 2 dark green bags.\n\
    \dark green bags contain 2 dark blue bags.\n\
    \dark blue bags contain 2 dark violet bags.\n\
    \dark violet bags contain no other bags.\n"

prop_countInsideUnit :: Property
prop_countInsideUnit = property $ countInside "shiny gold" testContains2 == 126

return []

runTests :: IO Bool
runTests = $quickCheckAll