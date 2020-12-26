{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Day25 where

import Data.List (find)
import Data.Maybe (fromJust)
import Math.NumberTheory.Powers.Modular (powMod)
import Test.QuickCheck (Property, ioProperty, property)
import Test.QuickCheck.All (quickCheckAll)

handshake :: Integer -> Integer -> Integer
handshake s n = powMod s n 20201227

loopSize :: Integer -> Integer
loopSize i = fromJust . find ((== i) . handshake 7) $ [1 ..]

findSecret :: (Integer, Integer) -> Integer
findSecret (pd, pc) = handshake pd (loopSize pc)

part1 :: IO ()
part1 = print . findSecret =<< input

-- Input handling

readKeys :: String -> (Integer, Integer)
readKeys s =
  let [k1, k2] = map read . lines $ s
   in (k1, k2)

input :: IO (Integer, Integer)
input = readKeys <$> readFile "data/Day25.txt"

-- Testing

prop_regression :: Property
prop_regression = ioProperty $ (== 16881444) . findSecret <$> input

testKeys :: (Integer, Integer)
testKeys = (17807724, 5764801)

prop_findSecretUnit :: Property
prop_findSecretUnit = property $ findSecret testKeys == 14897079

return []

runTests :: IO Bool
runTests = $quickCheckAll