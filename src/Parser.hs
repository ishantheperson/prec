{-# LANGUAGE BlockArguments #-}
module Parser (parseString) where 

import Prelude hiding (succ)

import Programs 

import Data.Char (toLower)

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Tok 

parseString = parse (whitespace >> program) "" . map toLower 

program :: Parser Program 
program = chainl1 term (try $ reserved "o" *> pure Compose)
    where term = prec <|> proj <|> natural <|> succ <|> tuple 

natural = Nat <$> integer  <?> "natural number"
succ = reserved "s" *> pure Succ <?> "S (successor)"
prec = (do 
  try $ reserved "prec"

  brackets do 
    h <- program 
 
    comma

    g <- program 
    return $ Prec h g) <?> "Prec"

proj = (do 
  try $ reserved "p"

  index <- integer 

  arity <- integer 

  return $ Proj index arity) <?> "P (proj)"

tuple = Tuple <$> parens (commaSep1 program) <?> "Tuple"

lexer = Tok.makeTokenParser (emptyDef {
  reservedNames = ["o", "p", "s", "prec"]
})

integer = Tok.natural lexer 
whitespace = Tok.whiteSpace lexer 
brackets = Tok.brackets lexer 
parens = Tok.parens lexer 
reserved = Tok.reserved lexer 
comma = Tok.comma lexer 
commaSep1 = Tok.commaSep1 lexer 