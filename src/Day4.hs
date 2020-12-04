{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Day4 where

import Control.Monad (guard)
import Data.Char (isSpace)
import Data.Either (rights)
import Data.Functor (($>))
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Test.QuickCheck (Arbitrary (..), Property, elements, ioProperty, listOf1, property, (===))
import Test.QuickCheck.All (quickCheckAll)
import Text.Parsec
  ( char,
    digit,
    eof,
    many1,
    parse,
    satisfy,
    space,
    string,
    try,
    (<|>),
  )
import Text.Parsec.Perm (permute, (<$$>), (<|?>), (<||>))
import Text.Parsec.String (Parser)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))

data Passport = Passport
  { byrField :: Int,
    iyrField :: Int,
    eyrField :: Int,
    hgtField :: String,
    hclField :: String,
    eclField :: String,
    pidField :: String,
    cidField :: Maybe String
  }
  deriving (Eq, Show)

newtype Batch = Batch {getBatch :: [Passport]}
  deriving (Eq, Show)

valid :: Passport -> Bool
valid (Passport byr iyr eyr hgt hcl ecl pid _) =
  (1920 <= byr && byr <= 2002)
    && (2010 <= iyr && iyr <= 2020)
    && (2020 <= eyr && eyr <= 2030)
    && checkHeight hgt
    && hcl =~ "^#[a-f0-9]{6}$"
    && ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    && pid =~ "^[0-9]{9}$"
  where
    checkHeight h = isJust $ do
      (_, _, _, [sn, unit]) <- pure (h =~ "^([0-9]+)(in|cm)$" :: (String, String, String, [String]))
      n <- readMaybe sn :: Maybe Int
      case unit of
        "cm" -> guard (150 <= n && n <= 193)
        "in" -> guard (59 <= n && n <= 76)
        _ -> Nothing

getValid :: Batch -> [Passport]
getValid = filter valid . getBatch

part1 :: IO ()
part1 = print . length . getBatch =<< input

part2 :: IO ()
part2 = print . length . getValid =<< input

-- Input handling

class Pretty a where
  pretty :: a -> String

instance Pretty Passport where
  pretty (Passport byr iyr eyr hgt hcl ecl pid cid) =
    "byr:" ++ show byr
      ++ " iyr:"
      ++ show iyr
      ++ " eyr:"
      ++ show eyr
      ++ " hgt:"
      ++ hgt
      ++ "\nhcl:"
      ++ hcl
      ++ " ecl:"
      ++ ecl
      ++ " pid:"
      ++ pid
      ++ ( case cid of
             Nothing -> ""
             Just x -> " cid:" ++ x
         )

instance Pretty Batch where
  pretty = intercalate "\n\n" . (pretty <$>) . getBatch

instance Read Batch where
  readsPrec _ str = [(parseBatch str, "")]
    where
      parseBatch =
        Batch
          . rights
          . map (parse parsePassport "")
          . splitOn "\n\n"

parsePassport :: Parser Passport
parsePassport =
  permute $
    Passport
      <$$> datum "byr" int
      <||> datum "iyr" int
      <||> datum "eyr" int
      <||> datum "hgt" str
      <||> datum "hcl" str
      <||> datum "ecl" str
      <||> datum "pid" str
      <|?> (Nothing, Just <$> datum "cid" str)
  where
    int = read <$> many1 digit
    str = many1 (satisfy (not . isSpace))

datum :: String -> Parser a -> Parser a
datum s p = try (string s *> char ':' *> p <* ((space $> ()) <|> eof))

input :: IO Batch
input = read <$> readFile "data/Day4.txt"

-- Testing

instance Arbitrary Batch where
  arbitrary = Batch <$> listOf1 arbitrary

instance Arbitrary Passport where
  arbitrary =
    Passport
      <$> genYear
      <*> genYear
      <*> genYear
      <*> genHeight
      <*> genColor
      <*> genColor
      <*> genId
      <*> genMaybeId
    where
      genYear = elements [2000 .. 2020]
      genHeight = (++) <$> (show <$> elements [100 .. 200 :: Int]) <*> elements ["cm", "in", ""]
      genColor = elements ["brn", "blk", "red", "#ffffff", "z", "foo"]
      genId = elements $ show <$> [0 .. 100 :: Int]
      genMaybeId = elements $ Nothing : (Just . show <$> [0 .. 100 :: Int])

prop_regression :: Property
prop_regression =
  ioProperty $
    (\b -> length (getBatch b) == 216 && length (getValid b) == 150) <$> input

prop_roundTrip :: Batch -> Property
prop_roundTrip ps = ps === read (pretty ps)

testBatch :: Batch
testBatch =
  read
    "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
    \byr:1937 iyr:2017 cid:147 hgt:183cm\n\
    \\n\
    \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
    \hcl:#cfa07d byr:1929\n\
    \\n\
    \hcl:#ae17e1 iyr:2013\n\
    \eyr:2024\n\
    \ecl:brn pid:760753108 byr:1931\n\
    \hgt:179cm\n\
    \n\
    \hcl:#cfa07d eyr:2025 pid:166559648\n\
    \iyr:2011 ecl:brn hgt:59in"

prop_part1Unit :: Property
prop_part1Unit = property $ length (getBatch testBatch) == 2

testInvalidBatch :: Batch
testInvalidBatch =
  read
    "eyr:1972 cid:100\n\
    \hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\
    \\n\
    \iyr:2019\n\
    \hcl:#602927 eyr:1967 hgt:170cm\n\
    \ecl:grn pid:012533040 byr:1946\n\
    \\n\
    \hcl:dab227 iyr:2012\n\
    \ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\
    \\n\
    \hgt:59cm ecl:zzz\n\
    \eyr:2038 hcl:74454a iyr:2023\n\
    \pid:3556412378 byr:2007"

testValidBatch :: Batch
testValidBatch =
  read
    "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\n\
    \hcl:#623a2f\n\
    \\n\
    \eyr:2029 ecl:blu cid:129 byr:1989\n\
    \iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\
    \\n\
    \hcl:#888785\n\
    \hgt:164cm byr:2001 iyr:2015 cid:88\n\
    \pid:545766238 ecl:hzl\n\
    \eyr:2022\n\
    \\n\
    \iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"

prop_part2Unit :: Property
prop_part2Unit =
  property $
    length (getValid testValidBatch) == 4 && null (getValid testInvalidBatch)

return []

runTests :: IO Bool
runTests = $quickCheckAll
