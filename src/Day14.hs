{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day14 where

import Control.Lens (at, makeLenses, over, set, view, (^.))
import Control.Monad (zipWithM)
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Test.QuickCheck (Property, ioProperty, property)
import Test.QuickCheck.All (quickCheckAll)
import Text.Parsec (char, count, digit, endOfLine, eof, many, many1, parse, string, try, (<|>))

type Addr = I36

newtype I36 = I36 {getBits :: [Bool]}
  deriving (Show, Eq, Ord)

newtype Mask36 = Mask36 {getMasks :: [Maybe Bool]}
  deriving (Show)

integerToI36 :: Integer -> I36
integerToI36 = I36 . reverse . aux (36 :: Int)
  where
    aux 0 _ = []
    aux n i = odd i : aux (n - 1) (i `div` 2)

i36ToInteger :: I36 -> Integer
i36ToInteger = foldl (\a d -> 2 * a + bit d) 0 . getBits
  where
    bit b = if b then 1 :: Integer else 0

applyValueMask :: Mask36 -> I36 -> I36
applyValueMask mask int = I36 $ zipWith fromMaybe (getBits int) (getMasks mask)

applyAddrMask :: Mask36 -> I36 -> [I36]
applyAddrMask mask int = map I36 $ zipWithM bitOptions (getBits int) (getMasks mask)
  where
    bitOptions _ Nothing = [True, False]
    bitOptions _ (Just True) = [True]
    bitOptions b (Just False) = [b]

data Instr = Mask Mask36 | Assign Addr I36
  deriving (Show)

data System = System {_bitMask :: Mask36, _memory :: Map Addr I36}
  deriving (Show)

makeLenses ''System

initSystem :: System
initSystem = System (Mask36 $ replicate 36 Nothing) Map.empty

type Update = Instr -> System -> System

update1 :: Update
update1 (Mask m) sys = set bitMask m sys
update1 (Assign a i) sys = set (memory . at a) (Just $ applyValueMask (sys ^. bitMask) i) sys

update2 :: Update
update2 (Mask m) sys = set bitMask m sys
update2 (Assign unmaskA i) sys =
  let as = applyAddrMask (sys ^. bitMask) unmaskA
   in over memory (Map.fromList ((,i) <$> as) `Map.union`) sys

simulate :: Update -> [Instr] -> System
simulate u = foldl (flip u) initSystem

sumMemory :: System -> Integer
sumMemory = sum . map i36ToInteger . Map.elems . view memory

part1 :: IO ()
part1 = print . sumMemory . simulate update1 =<< input

part2 :: IO ()
part2 = print . sumMemory . simulate update2 =<< input

-- Input handling

readMask :: String -> Mask36
readMask = Mask36 . map maskFromChar
  where
    maskFromChar 'X' = Nothing
    maskFromChar '0' = Just False
    maskFromChar '1' = Just True
    maskFromChar _ = error "bad mask"

readProgram :: String -> [Instr]
readProgram = either (error . show) id . parse parseInput ""
  where
    parseInput = many instr
    instr = (try mask <|> mem) <* (endOfLine $> () <|> eof)
    mask = Mask . readMask <$> (string "mask = " *> maskBits)
    maskBits = count 36 $ char 'X' <|> char '1' <|> char '0'
    mem = Assign <$> (string "mem[" *> int <* string "] = ") <*> int
    int = integerToI36 . read <$> many1 digit

input :: IO [Instr]
input = readProgram <$> readFile "data/Day14.txt"

-- Testing

prop_regression :: Property
prop_regression =
  ioProperty $
    ( \p ->
        sumMemory (simulate update1 p) == 9879607673316
          && sumMemory (simulate update2 p) == 3435342392262
    )
      <$> input

testProgram1 :: [Instr]
testProgram1 =
  readProgram
    "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
    \mem[8] = 11\n\
    \mem[7] = 101\n\
    \mem[8] = 0"

testProgram2 :: [Instr]
testProgram2 =
  readProgram
    "mask = 000000000000000000000000000000X1001X\n\
    \mem[42] = 100\n\
    \mask = 00000000000000000000000000000000X0XX\n\
    \mem[26] = 1"

prop_simulateUnit :: Property
prop_simulateUnit =
  property $
    sumMemory (simulate update1 testProgram1) == 165
      && sumMemory (simulate update2 testProgram2) == 208

return []

runTests :: IO Bool
runTests = $quickCheckAll