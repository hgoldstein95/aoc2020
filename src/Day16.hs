{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Day16 where

import Control.Lens (makeLenses, over, _2)
import Data.Functor (($>))
import Data.List (delete, intercalate, isPrefixOf, sortOn, transpose)
import Data.Maybe (fromJust, listToMaybe)
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    elements,
    getNonNegative,
    ioProperty,
    listOf,
    property,
    vectorOf,
    (===),
  )
import Test.QuickCheck.All (quickCheckAll)
import Text.Parsec
  ( char,
    digit,
    endOfLine,
    eof,
    letter,
    many,
    many1,
    parse,
    sepBy1,
    space,
    string,
    try,
    (<|>),
  )

newtype Field = Field {getField :: String}
  deriving (Show, Eq, Ord)

newtype TicketNo = TicketNo {getTicketNo :: Int}
  deriving (Show, Eq, Ord)

newtype Ticket = Ticket {getTicket :: [TicketNo]}
  deriving (Show, Eq, Ord)

newtype Range = Range (TicketNo, TicketNo)
  deriving (Show, Eq, Ord)

type Rule = (Field, (Range, Range))

data Notes = Notes
  { _rules :: [Rule],
    _yourTicket :: Ticket,
    _nearbyTickets :: [Ticket]
  }
  deriving (Eq, Show)

makeLenses ''Notes

inRange :: TicketNo -> Range -> Bool
inRange t (Range (tmin, tmax)) = tmin <= t && t <= tmax

checkRule :: TicketNo -> Rule -> Bool
checkRule t (_, (r1, r2)) = t `inRange` r1 || t `inRange` r2

scanningErrorRate :: Notes -> Int
scanningErrorRate (Notes rs _ ts) =
  sum . map getTicketNo . filter (\t -> not $ any (checkRule t) rs) . concatMap getTicket $ ts

isPossiblyValid :: [Rule] -> Ticket -> Bool
isPossiblyValid rs (Ticket ns) = all (\t -> any (checkRule t) rs) ns

fixNotes :: Notes -> Notes
fixNotes (Notes rs t ts) = Notes rs t (filter (isPossiblyValid rs) ts)

-- | Collapses lists of options into a single list that (1) is unique, and (2) satisfies the
-- | requirements of the original lists
collapse :: (Eq a) => [[a]] -> Maybe [a]
collapse =
  (map snd . sortOn fst <$>)
    . listToMaybe
    . aux
    . sortOn (length . snd)
    . zip [0 :: Int ..]
  where
    aux [] = pure []
    aux ((n, x) : xs) = x >>= \f -> ((n, f) :) <$> aux (map (over _2 (delete f)) xs)

sortFields :: Notes -> [Field]
sortFields (Notes rs _ ts) =
  fromJust
    . collapse
    . map (\ns -> map fst $ filter (\r -> all (`checkRule` r) ns) rs)
    . transpose
    . map getTicket
    $ ts

multiplyDepartures :: Notes -> Int
multiplyDepartures n@(Notes _ (Ticket ns) _) =
  let fs = sortFields n
   in product
        . map (getTicketNo . snd)
        . filter (isPrefixOf "departure" . getField . fst)
        $ zip fs ns

part1 :: IO ()
part1 = print . scanningErrorRate =<< input

part2 :: IO ()
part2 = print . multiplyDepartures . fixNotes =<< input

-- Input handling

class Pretty a where
  pretty :: a -> String

instance Pretty Ticket where
  pretty (Ticket m) = intercalate "," (pretty <$> m)

instance Pretty Field where
  pretty (Field f) = f

instance Pretty TicketNo where
  pretty (TicketNo n) = show n

instance Pretty Range where
  pretty (Range (t1, t2)) = pretty t1 ++ "-" ++ pretty t2

instance Pretty Notes where
  pretty (Notes rs t ts) =
    intercalate "\n" $
      (showRule <$> rs)
        ++ ["", "your ticket:"]
        ++ [pretty t]
        ++ ["", "nearby tickets:"]
        ++ (pretty <$> ts)
    where
      showRule (f, (r1, r2)) = pretty f ++ ": " ++ pretty r1 ++ " or " ++ pretty r2

instance Read Notes where
  readsPrec _ s = [(either (error . show) id . parse parseInput "" $ s, "")]
    where
      parseInput = do
        rs <- many (try rule)
        lineEnd *> string "your ticket:" *> lineEnd $> ()
        t <- ticket
        lineEnd *> string "nearby tickets:" *> lineEnd $> ()
        ts <- many ticket
        pure (Notes rs t ts)
      rule =
        ((,) <$> field <* string ": " <*> ((,) <$> range <* string " or " <*> range)) <* lineEnd
      field = Field <$> many1 (letter <|> space)
      range = Range <$> ((,) <$> ticketNo <* char '-' <*> ticketNo)
      ticketNo = TicketNo . read <$> many1 digit
      ticket =
        Ticket <$> (ticketNo `sepBy1` char ',') <* lineEnd
      lineEnd = endOfLine $> () <|> eof

input :: IO Notes
input = read <$> readFile "data/Day16.txt"

-- Testing

instance Arbitrary Field where
  arbitrary = Field <$> elements simpleWords

instance Arbitrary TicketNo where
  arbitrary = TicketNo . getNonNegative <$> arbitrary

instance Arbitrary Range where
  arbitrary = do
    (n1, n2) <- (,) <$> arbitrary <*> arbitrary
    return $ Range (min n1 n2, max n1 n2)

genTicket :: Int -> Gen Ticket
genTicket n = Ticket <$> vectorOf n arbitrary

uniqueVectorOf :: Eq a => Int -> Gen a -> Gen [a]
uniqueVectorOf 0 _ = pure []
uniqueVectorOf n g = uniqueVectorOf (n - 1) g >>= aux
  where
    aux vs = g >>= \v -> if v `elem` vs then aux vs else pure (v : vs)

instance Arbitrary Notes where
  arbitrary = do
    n <- elements [1 .. 5]
    fs <- uniqueVectorOf n arbitrary
    Notes
      <$> (zip fs <$> vectorOf n arbitrary)
      <*> genTicket n
      <*> listOf (genTicket n)

prop_regression :: Property
prop_regression =
  ioProperty $
    ( \n ->
        scanningErrorRate n == 21996
          && (multiplyDepartures . fixNotes) n == 650080463519
    )
      <$> input

prop_roundTrip :: Notes -> Property
prop_roundTrip ps = ps === read (pretty ps)

testNotes1 :: Notes
testNotes1 =
  read
    "class: 1-3 or 5-7\n\
    \row: 6-11 or 33-44\n\
    \seat: 13-40 or 45-50\n\
    \\n\
    \your ticket:\n\
    \7,1,14\n\
    \\n\
    \nearby tickets:\n\
    \7,3,47\n\
    \40,4,50\n\
    \55,2,20\n\
    \38,6,12"

prop_scanningErrorRateUnit :: Property
prop_scanningErrorRateUnit = property $ scanningErrorRate testNotes1 == 71

testNotes2 :: Notes
testNotes2 =
  read
    "class: 0-1 or 4-19\n\
    \row: 0-5 or 8-19\n\
    \seat: 0-13 or 16-19\n\
    \\n\
    \your ticket:\n\
    \11,12,13\n\
    \\n\
    \nearby tickets:\n\
    \3,9,18\n\
    \15,1,5\n\
    \5,14,9"

prop_sortFieldsUnit :: Property
prop_sortFieldsUnit =
  property $
    sortFields testNotes2 == [Field "row", Field "class", Field "seat"]

simpleWords :: [String]
simpleWords =
  [ "awake",
    "bad",
    "bent",
    "bitter",
    "blue",
    "certain",
    "cold",
    "complete",
    "cruel",
    "dark",
    "dead",
    "dear",
    "dirty",
    "dry",
    "false",
    "feeble",
    "female",
    "foolish",
    "future",
    "green",
    "ill",
    "last",
    "late",
    "left",
    "loose",
    "loud",
    "low",
    "mixed",
    "narrow",
    "old",
    "opposite",
    "public",
    "rough",
    "sad",
    "safe",
    "secret"
  ]

return []

runTests :: IO Bool
runTests = $quickCheckAll