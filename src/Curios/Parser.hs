module Curios.Parser
  ( space
  , lexeme
  , symbol
  , name
  , identifier
  , piBinding
  , lambdaBinding
  , literal
  , expression
  , statement
  , program
  )
  where

import Data.Void
  ( Void
  )

import Control.Applicative
  ( (<|>)
  )

import Text.Megaparsec
  ( Parsec (..)
  , optional
  , try
  , some
  , many
  , oneOf
  , single
  , manyTill
  , sepBy1
  )

import Text.Megaparsec.Char
  ( space1
  )

import qualified Text.Megaparsec.Char.Lexer as Lexer
  ( space
  , skipLineComment
  , skipBlockComment
  , lexeme
  , symbol
  , charLiteral
  , decimal
  , float
  )

import Curios.Expression
  ( Name (..)
  , PiBinding (..)
  , LambdaBinding (..)
  , Literal (..)
  , Identifier (..)
  , Expression (..)
  , Statement (..)
  , Program (..)
  )

type Parser a =
  Parsec Void String a

space :: Parser ()
space =
  Lexer.space space1 spLineComment spBlockComment where
    spLineComment = Lexer.skipLineComment "//"
    spBlockComment = Lexer.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme parser =
  Lexer.lexeme space parser

symbol :: String -> Parser String
symbol string =
  Lexer.symbol space string

name :: Parser Name
name =
  lexeme (Name <$> some (oneOf naValidCharacters)) where
    naValidCharacters =
      ['a'..'z'] ++
      ['A'..'Z'] ++
      ['+', '-', '*', '/', '=']

identifier :: Parser Identifier
identifier =
  lexeme (Identifier <$> sepBy1 name (single ';'))

piBinding :: Parser PiBinding
piBinding =
  lexeme (PiBinding <$> optional (try (name <* symbol ":")) <*> (expression <* symbol "."))

lambdaBinding :: Parser LambdaBinding
lambdaBinding =
  lexeme (LambdaBinding <$> name <*> optional (symbol ":" *> expression) <* symbol ".")

literal :: Parser Literal
literal =
  lexeme (liCharacter <|> liString <|> try liRational <|> liInteger) where
    liCharacter = LiCharacter <$> (single '\'' *> Lexer.charLiteral)
    liString = LiString <$> (single '"' *> manyTill Lexer.charLiteral (single '"'))
    liRational = raPositive <|> raNegative where
      raPositive = LiRational <$> (optional (single '+') *> Lexer.float)
      raNegative = (LiRational . negate) <$> (single '-' *> Lexer.float)
    liInteger = inPositive <|> inNegative where
      inPositive = LiInteger <$> (optional (single '+') *> Lexer.decimal)
      inNegative = (LiInteger . negate) <$> (single '-' *> Lexer.decimal)

expression :: Parser Expression
expression =
  lexeme (exVariable <|> exPiAbstraction <|> exLambdaAbstraction <|> exApplication <|> exLiteral) where
    exVariable = ExVariable <$> identifier
    exPiAbstraction = ExPiAbstraction <$> (symbol "<" *> some (try piBinding)) <*> (expression <* symbol ">")
    exLambdaAbstraction = ExLambdaAbstraction <$> (symbol "{" *> some (try lambdaBinding)) <*> (expression <* symbol "}")
    exApplication = ExApplication <$> (symbol "[" *> expression) <*> manyTill expression (symbol "]")
    exLiteral = ExLiteral <$> literal

statement :: Parser Statement
statement =
  lexeme (symbol "(" *> (stPackage <|> stImport <|> stAssume <|> stDefine) <* symbol ")") where
    stPackage = StPackage <$> (symbol "package" *> name) <*> program
    stImport = StImport <$> (symbol "import" *> identifier)
    stAssume = StAssume <$> (symbol "assume" *> name) <*> expression
    stDefine = StDefine <$> (symbol "define" *> name) <*> expression <*> expression

program :: Parser Program
program =
  lexeme (Program <$> many (try statement))