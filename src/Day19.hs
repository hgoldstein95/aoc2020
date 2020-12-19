{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day19 where

import Data.Functor (($>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.List.Split (splitOn)
import Test.QuickCheck (Property, ioProperty)
import Test.QuickCheck.All (quickCheckAll)
import Text.Parsec
  ( anyChar,
    char,
    digit,
    many1,
    parse,
    sepEndBy1,
    space,
    string,
    try,
    (<|>),
  )
import Text.ParserCombinators.ReadP (ReadP, (+++))
import qualified Text.ParserCombinators.ReadP as R

type Idx = Int

type Rules = IntMap Rule

data Rule = Lit Char | Concat [Idx] | Disj [Idx] [Idx]
  deriving (Eq, Show)

compile1 :: Rules -> ReadP ()
compile1 m = aux 0 >> R.eof
  where
    aux n = case m Map.! n of
      Lit c -> R.char c $> ()
      Concat xs -> con xs
      Disj xs ys -> con xs +++ con ys
    con = foldr ((>>) . aux) (pure ())

compile2 :: Rules -> ReadP ()
compile2 m = aux 0 >> R.eof
  where
    aux n = case m Map.! n of
      Lit c -> R.char c $> ()
      Concat [x] | n == 8 -> R.many1 (aux x) $> ()
      Concat [x, y] | n == 11 -> do
        l <- length <$> R.many1 (aux x)
        R.count l (aux y) $> ()
      Concat xs -> con xs
      Disj xs ys -> con xs +++ con ys
    con = foldr ((>>) . aux) (pure ())

match :: ReadP () -> String -> Bool
match p = not . null . R.readP_to_S p

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
readRule = either (error . show) id . parse indexedRule ""
  where
    indexedRule = (,) <$> idx <*> (string ": " *> rule)
    rule = lit <|> try disj <|> cat
    disj = Disj <$> idxs <*> (string "| " *> idxs)
    lit = Lit <$> (char '"' *> anyChar <* char '"')
    cat = Concat <$> idxs
    idxs = idx `sepEndBy1` space
    idx = read <$> many1 digit

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