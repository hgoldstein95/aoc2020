{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Day21 where

import Control.Arrow (Arrow (second))
import Data.List (intercalate, sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Test.QuickCheck (Property, ioProperty, property)
import Test.QuickCheck.All (quickCheckAll)
import qualified Text.Parser as P

type Ingredient = String

type Allergen = String

type Raw = [([Ingredient], [Allergen])]

analyzeAllergens :: Raw -> Map Allergen (Set Ingredient)
analyzeAllergens = Map.unionsWith Set.intersection . map (uncurry single)
  where
    single is as = Map.fromList . zip as . repeat . Set.fromList $ is

countSafe :: Raw -> Int
countSafe raw =
  let s = Set.unions . Map.elems . analyzeAllergens $ raw
   in length . filter (`Set.notMember` s) . concatMap fst $ raw

determineAllergens :: Map Allergen (Set Ingredient) -> [(Ingredient, Allergen)]
determineAllergens = head . aux . map (second (NE.fromList . Set.toList)) . Map.assocs
  where
    constrain i = mapM (\(a, is) -> ((a,) <$>) . NE.nonEmpty . NE.filter (/= i) $ is)
    aux (sortOn (length . snd) -> (a, is) : cs) = do
      i <- NE.toList is
      cs' <- maybeToList (constrain i cs)
      ((i, a) :) <$> aux cs'
    aux _ = [[]]

canonicalDanger :: Raw -> String
canonicalDanger =
  intercalate ","
    . map fst
    . sortOn snd
    . determineAllergens
    . analyzeAllergens

part1 :: IO ()
part1 = print . countSafe =<< input

part2 :: IO ()
part2 = print . canonicalDanger =<< input

-- Input handling

readRaw :: String -> Raw
readRaw = fromJust . P.parse (lists <* P.skipSpaces <* P.eof)
  where
    lists = list `P.sepBy` P.char '\n'
    list = (,) <$> ingrs <*> allergens
    ingrs = P.many P.alpha `P.sepBy1` P.space
    allergens = P.string " (contains " *> P.many P.alpha `P.sepBy1` P.string ", " <* P.char ')'

input :: IO Raw
input = readRaw <$> readFile "data/Day21.txt"

-- Testing

prop_regression :: Property
prop_regression =
  ioProperty $
    ( \r ->
        countSafe r == 2380
          && canonicalDanger r == "ktpbgdn,pnpfjb,ndfb,rdhljms,xzfj,bfgcms,fkcmf,hdqkqhh"
    )
      <$> input

testRaw :: Raw
testRaw =
  readRaw
    "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
    \trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
    \sqjhc fvjkl (contains soy)\n\
    \sqjhc mxmxvkd sbzzf (contains fish)"

prop_countSafeUnit :: Property
prop_countSafeUnit = property $ countSafe testRaw == 5

prop_canonicalDanger :: Property
prop_canonicalDanger = property $ canonicalDanger testRaw == "mxmxvkd,sqjhc,fvjkl"

return []

runTests :: IO Bool
runTests = $quickCheckAll