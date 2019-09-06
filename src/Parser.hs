{-# LANGUAGE BlockArguments #-}
module Parser (parseString) where 

import Programs 

import Data.Char (toLower)
import Data.Functor (($>))

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Tok 

type ParseResult = (Maybe String, Program)

-- | Parses a case insensitive string 
--   and returns its name, if it exists, 
--   and the associated function code 
parseString :: String -> Either ParseError ParseResult
parseString = parse file "" . map toLower 
  where file = do whitespace
                  name <- optionMaybe (try $ identifier <* reservedOp "=")
                  f <- program  

                  return (name, f)

program = chainl1 term (reserved "o" $> Compose)
    where term = prec <|> proj <|> natural <|> successor <|> tuple <|> ident

natural = Nat <$> integer  <?> "natural number"
successor = reserved "s" *> pure Succ <?> "S (successor)"
ident = Identifier <$> identifier <?> "identifier"

prec = (do 
  reserved "prec"

  brackets do 
    h <- program 
    comma
    g <- program 

    return $ Prec h g) <?> "Prec"

proj = (do 
  reserved "p"

  index <- integer 
  arity <- integer 

  return $ Proj index arity) <?> "P (proj)"

tuple = Tuple <$> parens (commaSep1 program) <?> "Tuple"

lexer = Tok.makeTokenParser (emptyDef {
  Tok.reservedNames = ["o", "p", "s", "prec"],
  Tok.identStart = letter,
  Tok.identLetter = alphaNum <|> char '\'',
  Tok.commentLine = "#"
})

integer = Tok.natural lexer 

whitespace = Tok.whiteSpace lexer 

brackets = Tok.brackets lexer 
parens = Tok.parens lexer 

comma = Tok.comma lexer 
commaSep1 = Tok.commaSep1 lexer 

reserved = Tok.reserved lexer 
reservedOp = Tok.reservedOp lexer 
identifier = Tok.identifier lexer 