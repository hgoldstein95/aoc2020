module Text.Parser
  ( module Text.ParserCombinators.ReadP,
    Parser,
    parse,
    any,
    digit,
    alpha,
    nat,
    space,
  )
where

import Data.List (find)
import Text.ParserCombinators.ReadP
  ( between,
    chainl,
    chainl1,
    chainr,
    chainr1,
    char,
    choice,
    count,
    endBy,
    endBy1,
    eof,
    gather,
    get,
    look,
    many,
    many1,
    manyTill,
    munch,
    munch1,
    option,
    optional,
    pfail,
    satisfy,
    sepBy,
    sepBy1,
    skipMany,
    skipMany1,
    skipSpaces,
    string,
    (+++),
    (<++),
  )
import qualified Text.ParserCombinators.ReadP as R
import Prelude hiding (any)

type Parser = R.ReadP

parse :: Parser a -> String -> Maybe a
parse p = (fst <$>) . find (null . snd) . R.readP_to_S p

any :: Parser Char
any = satisfy (const True)

digit :: Parser Char
digit = satisfy (`elem` ['0' .. '9'])

nat :: Parser Int
nat = read <$> many1 digit

alpha :: Parser Char
alpha = satisfy (`elem` ['a' .. 'z'] ++ ['A' .. 'Z'])

space :: Parser Char
space = satisfy (`elem` ['\n', '\r', '\t', ' '])
