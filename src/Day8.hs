{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Day8 where

import Control.Lens (makeLenses, view, (%=), (+=))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (StateT, gets, runStateT)
import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Test.QuickCheck (Arbitrary (..), Positive (..), Property, choose, ioProperty, oneof, property, (===))
import Test.QuickCheck.All (quickCheckAll)
import Text.Parsec (char, digit, endOfLine, eof, many, parse, string, (<|>))

(!!?) :: [a] -> Int -> Maybe a
xs !!? n = if n < 0 || n >= length xs then Nothing else Just (xs !! n)

data Inst = Acc Int | Jmp Int | Nop Int
  deriving (Eq, Show)

newtype Program = Program {instructions :: [Inst]}
  deriving (Eq, Show)

data Simulator = Sim {_executed :: Set Int, _accumulator :: Int, _counter :: Int}
  deriving (Eq, Show)

makeLenses ''Simulator

data RunResult = Loop Simulator | Term Simulator | Crash

getSimulator :: RunResult -> Simulator
getSimulator (Loop s) = s
getSimulator (Term s) = s
getSimulator _ = error "program crashed"

type M = StateT Simulator Maybe

runM :: M Bool -> RunResult
runM m =
  case runStateT m (Sim Set.empty 0 0) of
    Nothing -> Crash
    Just (True, s) -> Term s
    Just (False, s) -> Loop s

loadInst :: Int -> Program -> M Inst
loadInst n p = lift $ instructions p !!? n

simulate :: Program -> RunResult
simulate = runM . aux
  where
    aux :: Program -> M Bool
    aux p = do
      pc <- gets (view counter)
      past <- gets (view executed)
      case (pc `Set.member` past, pc == length (instructions p)) of
        (True, _) -> pure False
        (_, True) -> pure True
        _ -> do
          executed %= Set.insert pc
          execInst =<< loadInst pc p
          aux p

execInst :: Inst -> M ()
execInst (Acc i) = accumulator += i >> counter += 1
execInst (Jmp i) = counter += i
execInst (Nop _) = counter += 1

crashes :: Program -> Bool
crashes p = case simulate p of
  Crash -> True
  _ -> False

terminates :: Program -> Bool
terminates p = case simulate p of
  Term _ -> True
  _ -> False

fixes :: Program -> [Program]
fixes = map Program . aux . instructions
  where
    aux :: [Inst] -> [[Inst]]
    aux [] = []
    aux (Jmp i : is) = (Nop i : is) : ((Jmp i :) <$> aux is)
    aux (Nop i : is) = (Jmp i : is) : ((Nop i :) <$> aux is)
    aux (Acc i : is) = (Acc i :) <$> aux is

repair :: Program -> Program
repair = fromJust . find terminates . fixes

part1 :: IO ()
part1 = print . view accumulator . getSimulator . simulate =<< input

part2 :: IO ()
part2 = print . view accumulator . getSimulator . simulate . repair =<< input

-- Input handling

class Pretty a where
  pretty :: a -> String

instance Pretty Inst where
  pretty = \case
    Acc i -> "acc " ++ prettyInt i
    Jmp i -> "jmp " ++ prettyInt i
    Nop i -> "nop " ++ prettyInt i
    where
      prettyInt i = if i >= 0 then "+" ++ show i else show i

instance Pretty Program where
  pretty = unlines . map pretty . instructions

instance Read Program where
  readsPrec _ s = [(either (error . show) id . parse parseInput "" $ s, "")]
    where
      parseInput = Program <$> many inst
      inst = (acc <|> jmp <|> nop) <* (endOfLine $> () <|> eof)
      acc = Acc <$> (string "acc " *> int)
      jmp = Jmp <$> (string "jmp " *> int)
      nop = Nop <$> (string "nop " *> int)
      int = sign <*> (read <$> many digit)
      sign = (char '+' $> id) <|> (char '-' $> negate)

input :: IO Program
input = read <$> readFile "data/Day8.txt"

-- Testing

instance Arbitrary Program where
  arbitrary = do
    Positive n <- arbitrary
    Program <$> mapM (genInst n) [0 .. n]
    where
      genInst m i =
        oneof
          [ Acc <$> choose (-100, 100),
            Jmp <$> choose (- i, m - i),
            Nop <$> choose (-100, 100)
          ]

prop_regression :: Property
prop_regression =
  ioProperty $
    ( \p ->
        (view accumulator . getSimulator . simulate) p == 1939
          && (view accumulator . getSimulator . simulate . repair) p == 2212
    )
      <$> input

prop_roundTrip :: Program -> Property
prop_roundTrip ps = ps === read (pretty ps)

prop_arbitraryValid :: Program -> Property
prop_arbitraryValid ps = property $ not (crashes ps)

testProgram :: Program
testProgram =
  read
    "nop +0\n\
    \acc +1\n\
    \jmp +4\n\
    \acc +3\n\
    \jmp -3\n\
    \acc -99\n\
    \acc +1\n\
    \jmp -4\n\
    \acc +6"

prop_part1Unit :: Property
prop_part1Unit = property $ (view accumulator . getSimulator . simulate) testProgram == 5

prop_part2Unit :: Property
prop_part2Unit =
  property $
    (view accumulator . getSimulator . simulate) (repair testProgram) == 8

return []

runTests :: IO Bool
runTests = $quickCheckAll