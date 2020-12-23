{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day22 where

import Control.Monad.Trans.State.Strict (evalState, get, modify)
import Data.Either (isLeft)
import Data.Foldable (Foldable (toList))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Test.QuickCheck (Property, ioProperty, property)
import Test.QuickCheck.All (quickCheckAll)
import Text.Parser (parse)
import qualified Text.Parser as P

type Card = Int

type Deck = Seq Card

combat :: (Deck, Deck) -> Either Deck Deck
combat (d1, Empty) = Left d1
combat (Empty, d2) = Right d2
combat (c1 :<| cs1, c2 :<| cs2)
  | c1 > c2 = combat (cs2, cs1 |> c1 |> c2)
  | otherwise = combat (cs2 |> c2 |> c1, cs1)

memoFix :: Ord a => (forall m. Monad m => (a -> m b) -> a -> m b) -> a -> b
memoFix compute = (`evalState` Map.empty) . aux
  where
    aux x = get >>= maybe (computeAndSave x) pure . (Map.!? x)
    computeAndSave x = do
      k <- compute aux x
      modify (Map.insert x k)
      pure k

recCombat :: (Deck, Deck) -> Either Deck Deck
recCombat = memoFix (`aux` Set.empty)
  where
    aux _ prev ds@(d1, _)
      | ds `Set.member` prev = pure (Left d1)
    aux _ _ (d1, Empty) = pure (Left d1)
    aux _ _ (Empty, d2) = pure (Right d2)
    aux f prev ds@(c1 :<| cs1, c2 :<| cs2) =
      aux f (Set.insert ds prev) =<< do
        p1Wins <-
          if c1 <= length cs1 && c2 <= length cs2
            then isLeft <$> f (Seq.take c1 cs1, Seq.take c2 cs2)
            else pure (c1 > c2)
        pure $ if p1Wins then (cs1 |> c1 |> c2, cs2) else (cs1, cs2 |> c2 |> c1)

score :: Either Deck Deck -> Int
score = sum . zipWith (*) [1 ..] . toList . Seq.reverse . either id id

part1 :: IO ()
part1 = print . score . combat =<< input

part2 :: IO ()
part2 = print . score . recCombat =<< input

-- Input handling

readDecks :: String -> (Deck, Deck)
readDecks = fromJust . parse decks
  where
    decks = (,) <$> deck <*> deck
    deck = (header *> pcards) <* P.skipSpaces
    header = P.string "Player " *> P.nat <* P.string ":\n"
    pcards = Seq.fromList <$> P.nat `P.sepBy1` P.char '\n'

input :: IO (Deck, Deck)
input = readDecks <$> readFile "data/Day22.txt"

-- Testing

prop_regression :: Property
prop_regression =
  ioProperty $
    (\ds -> (score . combat) ds == 33421 && (score . recCombat) ds == 33651)
      <$> input

testDecks1 :: (Deck, Deck)
testDecks1 =
  readDecks
    "Player 1:\n\
    \9\n\
    \2\n\
    \6\n\
    \3\n\
    \1\n\
    \\n\
    \Player 2:\n\
    \5\n\
    \8\n\
    \4\n\
    \7\n\
    \10"

prop_combatUnit :: Property
prop_combatUnit = property $ (score . combat) testDecks1 == 306

testDecks2 :: (Deck, Deck)
testDecks2 =
  readDecks
    "Player 1:\n\
    \43\n\
    \19\n\
    \\n\
    \Player 2:\n\
    \2\n\
    \29\n\
    \14"

prop_recCombatUnit :: Property
prop_recCombatUnit =
  property $
    (score . recCombat) testDecks1 == 291
      && (score . recCombat) testDecks2 > 0

return []

runTests :: IO Bool
runTests = $quickCheckAll