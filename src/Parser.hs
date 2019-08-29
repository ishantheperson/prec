{-# LANGUAGE BlockArguments, LambdaCase #-}
module Parser where 

import Prelude hiding (succ)

import Data.Char (toLower)
import Data.List (intercalate)

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Tok 

data Program = 
             -- | Represents hard natural number constants 
               Nat Integer 
             | Succ 
             -- | Projection Index Arity
             | Proj Integer Integer 
             -- | Compose 
             | Compose Program Program 
             | Prec Program Program 
             | Tuple [Program] 

instance Show Program where 
  show = \case 
    Nat i -> show i 
    Succ -> "S"
    Proj index arity -> "Proj(" ++ show index ++ ", " ++ show arity ++ ")"
    Compose f g -> "(" ++ show f ++ " o " ++ show g ++ ")"
    Prec h g -> "Prec[" ++ show h ++ ", " ++ show g ++ "]"
    Tuple xs -> "(" ++ intercalate ", " (map show xs) ++ ")"


parseString = parse program "" . map toLower 

program :: Parser Program 
program = chainl1 term (whitespace >> (string "o " *> pure Compose))
    where term = prec <|> proj <|> natural <|> succ <|> tuple 


natural = Nat <$> integer 
succ = string "s" *> pure Succ
prec = do 
  try $ string "prec"

  whitespace 

  brackets do 
    h <- program 

    whitespace 
    string ","
    whitespace 

    g <- program 
    return $ Prec h g 

proj = do 
  try $ string "p"

  whitespace 

  index <- integer 

  whitespace 

  arity <- integer 

  return $ Proj index arity 

tuple = Tuple <$> parens (sepBy1 program (string "," >> whitespace))

lexer = Tok.makeTokenParser emptyDef 
integer = Tok.integer lexer 
whitespace = Tok.whiteSpace lexer 
brackets = Tok.brackets lexer 
parens = Tok.parens lexer 
