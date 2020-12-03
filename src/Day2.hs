{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Day2 where

import Test.QuickCheck (Arbitrary (..), Property, elements, ioProperty, property, vectorOf, (===))
import Test.QuickCheck.All (quickCheckAll)
import Text.Parsec
  ( anyChar,
    char,
    digit,
    endOfLine,
    many,
    many1,
    manyTill,
    parse,
    space,
    string,
  )

newtype Password = Password {unPassword :: String}
  deriving (Eq)

instance Show Password where
  show = unPassword

data Policy = Policy
  { frequencies :: (Int, Int),
    passwordChar :: Char
  }
  deriving (Eq)

instance Show Policy where
  show (Policy (x, y) c) = show x ++ "-" ++ show y ++ " " ++ [c]

type ValidCheck = Policy -> Password -> Bool

valid1 :: ValidCheck
valid1 (Policy (lo, hi) c) (Password p) =
  (\l -> lo <= l && l <= hi)
    . length
    . filter (== c)
    $ p

valid2 :: ValidCheck
valid2 (Policy (x, y) c) (Password p) = (p !! (x - 1) == c) /= (p !! (y - 1) == c)

countValid :: ValidCheck -> Database -> Int
countValid v = length . filter (uncurry v) . getDatabase

part1 :: IO ()
part1 = print . countValid valid1 =<< input

part2 :: IO ()
part2 = print . countValid valid2 =<< input

-- Input handling

newtype Database = Database {getDatabase :: [(Policy, Password)]}
  deriving (Eq, Arbitrary)

instance Show Database where
  show = unlines . map (\(p, s) -> show p ++ ": " ++ show s) . getDatabase

instance Read Database where
  readsPrec _ s = [(either (error . show) id . parse parseInput "" $ s, "")]
    where
      parseInput = Database <$> many line
      line = (,) <$> policy <* string ": " <*> (Password <$> manyTill anyChar endOfLine)
      policy = Policy <$> freqs <* space <*> anyChar
      freqs = (,) <$> (int <* char '-') <*> int
      int = read <$> many1 digit

input :: IO Database
input = read <$> readFile "data/Day2.txt"

-- Testing

instance Arbitrary Password where
  arbitrary = do
    l <- elements [1, 20]
    Password <$> vectorOf l (elements ['a' .. 'z'])

instance Arbitrary Policy where
  arbitrary = Policy <$> range <*> elements ['a' .. 'z']
    where
      range = do
        x <- elements [1 .. 5]
        y <- elements [x + 1 .. 6]
        pure (x, y)

prop_regression :: Property
prop_regression =
  ioProperty $
    (\xs -> countValid valid1 xs == 569 && countValid valid2 xs == 346) <$> input

prop_roundTrip :: Database -> Property
prop_roundTrip ps = ps === read (show ps)

prop_valid1Unit :: Property
prop_valid1Unit =
  property $
    valid1 (Policy (1, 3) 'c') (Password "cc")
      && valid1 (Policy (1, 3) 'c') (Password "aaacbcdfsjk")
      && not (valid1 (Policy (1, 3) 'c') (Password "cccc"))

prop_valid2Unit :: Property
prop_valid2Unit =
  property $
    valid2 (Policy (1, 3) 'a') (Password "abcde")
      && not (valid2 (Policy (1, 3) 'b') (Password "cdefg"))
      && not (valid2 (Policy (2, 9) 'c') (Password "ccccccccc"))

return []

runTests :: IO Bool
runTests = $quickCheckAll