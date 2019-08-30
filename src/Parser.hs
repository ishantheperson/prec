{-# LANGUAGE BlockArguments #-}
module Parser where 

import Prelude hiding (succ)

import Programs 

import Data.Char (toLower)

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Tok 

parseString = parse program "" . map toLower 

program :: Parser Program 
program = chainl1 term (try $ string " o " *> pure Compose)
    where term = prec <|> proj <|> natural <|> succ <|> tuple 

natural = Nat <$> integer  <?> "natural number"
succ = string "s" *> pure Succ <?> "S (successor)"
prec = (do 
  try $ string "prec"

  whitespace 

  brackets do 
    h <- program 

    whitespace 
    string ","
    whitespace 

    g <- program 
    return $ Prec h g) <?> "Prec"

proj = (do 
  try $ string "p"

  whitespace 

  index <- integer 

  whitespace 

  arity <- integer 

  return $ Proj index arity) <?> "P (proj)"

tuple = Tuple <$> parens (sepBy1 program (string "," >> whitespace)) <?> "Tuple"

lexer = Tok.makeTokenParser emptyDef 
integer = Tok.natural lexer 
whitespace = Tok.whiteSpace lexer 
brackets = Tok.brackets lexer 
parens = Tok.parens lexer 
