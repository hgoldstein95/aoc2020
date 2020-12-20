{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day19 where

import Data.Functor (($>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust)
import Test.QuickCheck (Property, ioProperty)
import Test.QuickCheck.All (quickCheckAll)
import Text.Parser (Parser, parse, (+++), (<++))
import qualified Text.Parser as P

type Idx = Int

type Rules = IntMap Rule

data Rule = Lit Char | Concat [Idx] | Disj [Idx] [Idx]
  deriving (Eq, Show)

compile1 :: Rules -> Parser ()
compile1 m = aux 0 >> P.eof
  where
    aux n = case m Map.! n of
      Lit c -> P.char c $> ()
      Concat xs -> con xs
      Disj xs ys -> con xs +++ con ys
    con = foldr ((>>) . aux) (pure ())

compile2 :: Rules -> Parser ()
compile2 m = aux 0 >> P.eof
  where
    aux n = case m Map.! n of
      Lit c -> P.char c $> ()
      Concat [x] | n == 8 -> P.many1 (aux x) $> ()
      Concat [x, y] | n == 11 -> do
        l <- length <$> P.many1 (aux x)
        P.count l (aux y) $> ()
      Concat xs -> con xs
      Disj xs ys -> con xs +++ con ys
    con = foldr ((>>) . aux) (pure ())

match :: Parser () -> String -> Bool
match p = isJust . parse p

part1 :: IO ()
part1 = do
  (rs, ws) <- input
  let r = compile1 rs
  print . length . filter (match r) $ ws

part2 :: IO ()
part2 = do
  (rs, ws) <- input
  let r = compile2 rs
  print . length . filter (match r) $ ws

-- Input handling

readRules :: [String] -> Rules
readRules = Map.fromList . map readRule

readRule :: String -> (Idx, Rule)
readRule = fromJust . parse (indexedRule <* P.eof)
  where
    indexedRule = (,) <$> P.nat <* P.string ": " <*> rule
    rule = disj <++ cat <++ lit
    disj = Disj <$> idxs <* P.string " | " <*> idxs
    lit = Lit <$> P.between (P.char '"') (P.char '"') P.any
    cat = Concat <$> idxs
    idxs = P.nat `P.sepBy1` P.char ' '

input :: IO (Rules, [String])
input = do
  [rs, is] <- splitOn [""] . lines <$> readFile "data/Day19.txt"
  pure (readRules rs, is)

-- Testing

prop_regression :: Property
prop_regression = ioProperty $ do
  (rs, ws) <- input
  let r1 = compile1 rs
  let r2 = compile2 rs
  pure $
    (length . filter (match r1)) ws == 235
      && (length . filter (match r2)) ws == 379

return []

runTests :: IO Bool
runTests = $quickCheckAll