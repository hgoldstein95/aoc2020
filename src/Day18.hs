{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Day18 where

import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Test.QuickCheck (Arbitrary (..), Property, frequency, getPositive, ioProperty, property, sized, (===))
import Test.QuickCheck.All (quickCheckAll)
import Text.Parsec (char, digit, many1, parse, string, (<|>))
import Text.Parsec.Expr
  ( Assoc (..),
    Operator (Infix),
    OperatorTable,
    buildExpressionParser,
  )
import Text.Parsec.String (Parser)

{-# ANN module "HLint: ignore Reduce duplication" #-}

data Expr = Lit Integer | Add Expr Expr | Mul Expr Expr
  deriving (Eq, Show)

newtype PrecExpr = PrecExpr {getExpr :: Expr}

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

part1 :: IO ()
part1 = print . sum . map eval =<< input

part2 :: IO ()
part2 = print . sum . map (eval . getExpr) =<< input

-- Input handling

class Pretty a where
  pretty :: a -> String
  pretty = prettyPrec (-1)

  prettyPrec :: Int -> a -> String
  prettyPrec _ = pretty

parenIf :: Bool -> String -> String
parenIf True = ("(" ++) . (++ ")")
parenIf False = id

instance Pretty Expr where
  prettyPrec _ (Lit n) = show n
  prettyPrec p (Add e1 e2) = parenIf (p > 1) $ prettyPrec 1 e1 ++ " + " ++ prettyPrec 2 e2
  prettyPrec p (Mul e1 e2) = parenIf (p > 1) $ prettyPrec 1 e1 ++ " * " ++ prettyPrec 2 e2

instance Pretty PrecExpr where
  prettyPrec prec = aux prec . getExpr
    where
      aux _ (Lit n) = show n
      aux p (Add e1 e2) = parenIf (p > 2) $ aux 1 e1 ++ " + " ++ aux 2 e2
      aux p (Mul e1 e2) = parenIf (p > 1) $ aux 3 e1 ++ " * " ++ aux 4 e2

parseExpr :: OperatorTable String () Identity Expr -> Parser Expr
parseExpr ops = buildExpressionParser table term
  where
    term = parens (parseExpr ops) <|> (Lit <$> nat)
    table = ops
    nat = read <$> many1 digit
    parens p = char '(' *> p <* char ')'

binaryExpr :: String -> (a -> a -> a) -> Assoc -> Operator String () Identity a
binaryExpr name f = Infix (string name $> f)

instance Read Expr where
  readsPrec _ s = [(either (error . show) id . parse expr "" . concat . words $ s, "")]
    where
      expr =
        parseExpr
          [[binaryExpr "*" Mul AssocLeft, binaryExpr "+" Add AssocLeft]]

instance Read PrecExpr where
  readsPrec _ s =
    [(either (error . show) id . parse (PrecExpr <$> expr) "" . concat . words $ s, "")]
    where
      expr =
        parseExpr
          [ [binaryExpr "+" Add AssocLeft],
            [binaryExpr "*" Mul AssocLeft]
          ]

input :: Read e => IO [e]
input = map read . lines <$> readFile "data/Day18.txt"

-- Testing

instance Arbitrary Expr where
  arbitrary = sized genExpr
    where
      genLit = Lit . getPositive <$> arbitrary
      genExpr 0 = genLit
      genExpr n =
        frequency
          [ (1, genLit),
            (n `div` 2, Add <$> genExpr (n `div` 5) <*> genExpr (4 * n `div` 5)),
            (n `div` 2, Mul <$> genExpr (n `div` 5) <*> genExpr (4 * n `div` 5))
          ]

prop_regression :: Property
prop_regression =
  ioProperty $
    (\es -> (sum . map eval) es == 75592527415659) <$> input

prop_roundTrip :: Expr -> Property
prop_roundTrip ps = ps === read (pretty ps)

prop_evalUnit :: Property
prop_evalUnit =
  property $
    (eval . read) "2 * 3 + (4 * 5)" == 26
      && (eval . read) "5 + (8 * 3 + 9 + 3 * 4 * 3)" == 437
      && (eval . read) "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" == 12240
      && (eval . read) "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" == 13632

prop_evalPrecUnit :: Property
prop_evalPrecUnit =
  property $
    (eval . getExpr . read) "1 + (2 * 3) + (4 * (5 + 6))" == 51
      && (eval . getExpr . read) "2 * 3 + (4 * 5)" == 46
      && (eval . getExpr . read) "5 + (8 * 3 + 9 + 3 * 4 * 3)" == 1445
      && (eval . getExpr . read) "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" == 669060
      && (eval . getExpr . read) "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" == 23340

return []

runTests :: IO Bool
runTests = $quickCheckAll