module Curios.Source.Parser
  (identifier
  ,literal
  ,functionTypeVariable
  ,functionVariable
  ,expression
  ,statement
  ,program
  )
  where

import Text.Megaparsec.Char (space1)
import Data.Void (Void)

import Text.Megaparsec.Debug (dbg)

import Curios.Source.Types
  (Identifier (..)
  ,Literal (..)
  ,FunctionTypeVariable (..)
  ,FunctionVariable (..)
  ,Expression (..)
  ,Statement (..)
  ,Program (..)
  )

import Text.Megaparsec
  (Parsec
  ,getSourcePos
  ,optional
  ,try
  ,some
  ,oneOf
  ,single
  ,manyTill
  ,sepBy
  ,eof
  ,(<|>)
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

identifier :: Parser Identifier
identifier =
  lexeme (Identifier <$> getSourcePos <*> some (try (oneOf idValidCharacters))) where
    idValidCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['+', '-', '*', '/', '=', '>', '<', '\'', '_', '.']

literal :: Parser Literal
literal =
  lexeme (ltText <|> try ltReal <|> ltInteger) where
    ltText = LtText <$> getSourcePos <*> (single '"' *> manyTill Lexer.charLiteral (single '"'))
    ltReal = rlPositive <|> rlNegative where
      rlPositive = LtReal <$> getSourcePos <*> (optional (single '+') *> Lexer.float)
      rlNegative = LtReal <$> getSourcePos <*> (single '-' *> (negate <$> Lexer.float))
    ltInteger = inPositive <|> inNegative where
      inPositive = LtInteger <$> getSourcePos <*> (optional (single '+') *> Lexer.decimal)
      inNegative = LtInteger <$> getSourcePos <*> (single '-' *> (negate <$> Lexer.decimal))

functionTypeVariable :: Parser FunctionTypeVariable
functionTypeVariable =
  lexeme (FunctionTypeVariable <$> getSourcePos <*> optional (try (identifier <* symbol ":")) <*> expression)

functionVariable :: Parser FunctionVariable
functionVariable =
  lexeme (FunctionVariable <$> getSourcePos <*> identifier)

expression :: Parser Expression
expression =
  lexeme (try exLiteral <|> exFunctionType <|> exFunction <|> try exApplication <|> exIdentifier) where
    exLiteral =
      ExLiteral <$> getSourcePos <*> literal
    exIdentifier =
      ExIdentifier <$> getSourcePos <*> identifier
    exFunctionType =
      ExFunctionType <$> getSourcePos <*>
        (symbol "->" *> optional (try identifier)) <*>
        (symbol "{" *> some (try (functionTypeVariable <* symbol ","))) <*>
        (expression <* symbol "}")
    exFunction =
      ExFunction <$> getSourcePos <*>
        (symbol "fn" *> symbol "{" *> some (try (functionVariable <* symbol ","))) <*>
        (expression <* symbol "}")
    exApplication =
      ExApplication <$> getSourcePos <*>
        (exIdentifier <* symbol "(") <*>
        (sepBy expression (symbol ",") <* symbol ")")

statement :: Parser Statement
statement =
  lexeme (stLet) where
    stLet =
      StLet <$> getSourcePos <*>
        (symbol "let" *> identifier) <*>
        (symbol ":" *> expression) <*>
        (symbol "=" *> expression <* symbol "end")

program :: Parser Program
program =
  lexeme (Program <$> getSourcePos <*> some statement <* eof)
