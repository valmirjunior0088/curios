module Curios.Syntax.Parser
  (name
  ,primitive
  ,functionTypeVariable
  ,functionVariable
  ,prefix
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
  ,FunctionTypeVariable (..)
  ,FunctionVariable (..)
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

functionTypeVariable :: Parser FunctionTypeVariable
functionTypeVariable =
  lexeme
    (FunctionTypeVariable <$>
      optional (try (name <* symbol "|")) <*>
      optional (try (name <* symbol ":")) <*>
      expression
    )

functionVariable :: Parser FunctionVariable
functionVariable =
  lexeme (FunctionVariable <$> name)

prefix :: Parser a -> Parser [a]
prefix parser =
  lexeme (some (try (parser <* symbol ",")))

expression :: Parser Expression
expression =
  lexeme (exName <|> exPrimitive <|> try exFunctionType <|> try exFunction <|> exApplication) where
    exName =
      ExName <$> name
    exPrimitive =
      ExPrimitive <$> primitive
    exFunctionType =
      ExFunctionType <$>
        (symbol "(" *> symbol "->" *> prefix functionTypeVariable) <*>
        (expression <* symbol ")")
    exFunction =
      ExFunction <$>
        (symbol "(" *> symbol "fn" *> prefix functionVariable) <*>
        (expression <* symbol ")")
    exApplication =
      ExApplication <$>
        (symbol "(" *> expression) <*>
        (manyTill expression (symbol ")"))

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
  lexeme (Program <$> some statement)
