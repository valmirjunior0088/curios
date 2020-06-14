module Curios.Parsing
  (literal
  ,name
  ,binding
  ,bindings
  ,expression
  ,statement
  )
  where

import Curios.Expression
  (Literal (..)
  ,Name (..)
  ,Binding (..)
  ,Expression (..)
  ,Statement (..)
  )

import Data.Void
  (Void
  )

import Text.Megaparsec
  (Parsec
  ,optional
  ,try
  ,some
  ,oneOf
  ,single
  ,manyTill
  ,(<|>)
  )

import Text.Megaparsec.Char
  (space1
  )

import qualified Text.Megaparsec.Char.Lexer as Lexer
  (space
  ,skipLineComment
  ,skipBlockComment
  ,lexeme
  ,symbol
  ,charLiteral
  ,decimal
  ,float
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

literal :: Parser Literal
literal =
  lexeme (ltCharacter <|> ltText <|> try ltRational <|> ltInteger) where
    ltCharacter = LtCharacter <$> (single '\'' *> Lexer.charLiteral <* single '\'')
    ltText = LtText <$> (single '"' *> manyTill Lexer.charLiteral (single '"'))
    ltRational = raPositive <|> raNegative where
      raPositive = LtRational <$> (optional (single '+') *> Lexer.float)
      raNegative = (LtRational . negate) <$> (single '-' *> Lexer.float)
    ltInteger = inPositive <|> inNegative where
      inPositive = LtInteger <$> (optional (single '+') *> Lexer.decimal)
      inNegative = (LtInteger . negate) <$> (single '-' *> Lexer.decimal)

name :: Parser Name
name =
  lexeme (Name <$> some (try (oneOf naValidCharacters))) where
    naValidCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ ['+', '-', '*', '/', '=', '\'']

binding :: Parser Binding
binding =
  lexeme (Binding <$> name <*> (symbol ":" *> expression))

bindings :: Parser [Binding]
bindings =
  lexeme (some (try (binding <* symbol ",")))

expression :: Parser Expression
expression =
  lexeme (try exLiteral <|> exVariable <|> exPiAbstraction <|> exLambdaAbstraction <|> exApplication) where
    exLiteral = ExLiteral <$> literal
    exVariable = ExVariable <$> name
    exPiAbstraction = ExPiAbstraction <$> (symbol "[" *> bindings) <*> (expression <* symbol "]")
    exLambdaAbstraction = ExLambdaAbstraction <$> (symbol "{" *> bindings) <*> (expression <* symbol "}")
    exApplication = ExApplication <$> (symbol "(" *> expression) <*> (manyTill expression (symbol ")"))

statement :: Parser Statement
statement =
  lexeme (stDef) where
    stDef = StDef <$> (symbol "def" *> name) <*> (expression <* symbol "end")
