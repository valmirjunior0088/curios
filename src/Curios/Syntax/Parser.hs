module Curios.Syntax.Parser
  (name
  ,primitive
  ,dependentVariable
  ,variable
  ,multiple
  ,expression
  ,statement
  ,program
  )
  where

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

import Curios.Syntax.Expression
  (Name (..)
  ,Primitive (..)
  ,DependentVariable (..)
  ,Variable (..)
  ,Expression (..)
  ,Statement (..)
  ,Program (..)
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
  lexeme (Name <$> some (try (oneOf nmValidCharacters))) where
    nmValidCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ ['+', '*', '/', '=', '\'', '_']

primitive :: Parser Primitive
primitive =
  lexeme (prText <|> try prRational <|> prInteger) where
    prText = PrText <$> (single '"' *> manyTill Lexer.charLiteral (single '"'))
    prRational = rtPositive <|> rtNegative where
      rtPositive = PrRational <$> (optional (single '+') *> Lexer.float)
      rtNegative = (PrRational . negate) <$> (single '-' *> Lexer.float)
    prInteger = inPositive <|> inNegative where
      inPositive = PrInteger <$> (optional (single '+') *> Lexer.decimal)
      inNegative = (PrInteger . negate) <$> (single '-' *> Lexer.decimal)

dependentVariable :: Parser DependentVariable
dependentVariable =
  lexeme
    (DependentVariable <$>
      (optional (try (name <* symbol "|"))) <*>
      (optional (try (name <* symbol ":"))) <*>
      expression
    )

variable :: Parser Variable
variable =
  lexeme (Variable <$> name)

multiple :: Parser a -> Parser [a]
multiple parser =
  lexeme (some (try (parser <* symbol ",")))

expression :: Parser Expression
expression =
  lexeme (exName <|> exPrimitive <|> exFunctionType <|> exFunction <|> exApplication) where
    exName = ExName <$> name
    exPrimitive = ExPrimitive <$> primitive
    exFunctionType = ExFunctionType <$> (symbol "[" *> multiple dependentVariable) <*> (expression <* symbol "]")
    exFunction = ExFunction <$> (symbol "{" *> multiple variable) <*> (expression <* symbol "}")
    exApplication = ExApplication <$> (symbol "(" *> expression) <*> (manyTill expression (symbol ")"))

statement :: Parser Statement
statement =
  lexeme
    (Statement <$>
      (symbol "let" *> name) <*>
      (symbol ":" *> expression) <*>
      (symbol "=" *> expression <* symbol "end")
    )

program :: Parser Program
program =
  lexeme (Program <$> some (try statement))
