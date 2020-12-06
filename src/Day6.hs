{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Day6 where

import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set
import Test.QuickCheck (Property, ioProperty, property)
import Test.QuickCheck.All (quickCheckAll)

type YesAnswer = Char

type Group = [Set YesAnswer]

countAnyoneYes :: Group -> Int
countAnyoneYes = length . foldr1 Set.union

countEveryoneYes :: Group -> Int
countEveryoneYes = length . foldr1 Set.intersection

part1 :: IO ()
part1 = print . sum . map countAnyoneYes =<< input

part2 :: IO ()
part2 = print . sum . map countEveryoneYes =<< input

-- Input handling

readGroups :: String -> [Group]
readGroups = map (map Set.fromList . lines) . splitOn "\n\n"

input :: IO [Group]
input = readGroups <$> readFile "data/Day6.txt"

-- Testing

prop_regression :: Property
prop_regression =
  ioProperty $
    ( \gs ->
        (sum . map countAnyoneYes) gs == 6947
          && (sum . map countEveryoneYes) gs == 3398
    )
      <$> input

testGroups :: [Group]
testGroups =
  readGroups
    "abc\n\
    \\n\
    \a\n\
    \b\n\
    \c\n\
    \\n\
    \ab\n\
    \ac\n\
    \\n\
    \a\n\
    \a\n\
    \a\n\
    \a\n\
    \\n\
    \b"

prop_countAnyoneYesUnit :: Property
prop_countAnyoneYesUnit = property $ map countAnyoneYes testGroups == [3, 3, 3, 1, 1]

prop_countEveryoneYesUnit :: Property
prop_countEveryoneYesUnit = property $ map countEveryoneYes testGroups == [3, 0, 1, 1, 1]

return []

runTests :: IO Bool
runTests = $quickCheckAll