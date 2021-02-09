module Curios.Source.Parser
  (identifier
  ,literal
  ,functionTypeVariable
  ,functionVariable
  ,expression
  ,binding
  ,prefix
  ,statement
  ,program
  )
  where

import Text.Megaparsec.Char (space1)
import Data.Void (Void)

import Curios.Source.Types
  (Identifier (..)
  ,Literal (..)
  ,FunctionTypeVariable (..)
  ,FunctionVariable (..)
  ,Expression (..)
  ,Binding (..)
  ,Prefix (..)
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
  lexeme (Identifier <$> getSourcePos <*> some (try (oneOf validCharacters))) where
    validCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['+', '-', '*', '/', '=', '>', '<', '\'', '_', '.']

literal :: Parser Literal
literal =
  lexeme (ltText <|> try ltReal <|> ltInteger) where
    ltText = LtText <$> getSourcePos <*> (single '"' *> manyTill Lexer.charLiteral (single '"'))
    ltReal = positive <|> negative where
      positive = LtReal <$> getSourcePos <*> (optional (single '+') *> Lexer.float)
      negative = LtReal <$> getSourcePos <*> (single '-' *> (negate <$> Lexer.float))
    ltInteger = positive <|> negative where
      positive = LtInteger <$> getSourcePos <*> (optional (single '+') *> Lexer.decimal)
      negative = LtInteger <$> getSourcePos <*> (single '-' *> (negate <$> Lexer.decimal))

functionTypeVariable :: Parser FunctionTypeVariable
functionTypeVariable =
  lexeme (FunctionTypeVariable <$> getSourcePos <*> optional (try (identifier <* symbol ":")) <*> expression)

functionVariable :: Parser FunctionVariable
functionVariable =
  lexeme (FunctionVariable <$> getSourcePos <*> identifier)

expression :: Parser Expression
expression =
  lexeme (exFunctionType <|> exFunction <|> exApplication <|> exLiteral <|> exIdentifier) where
    exFunctionType =
      ExFunctionType <$> getSourcePos <*>
        (try (symbol "->") *> optional identifier) <*>
        (symbol "{" *> some (try (functionTypeVariable <* symbol ","))) <*>
        (expression <* symbol "}")
    exFunction =
      ExFunction <$> getSourcePos <*>
        (try (symbol "fn") *> symbol "{" *> some (try (functionVariable <* symbol ","))) <*>
        (expression <* symbol "}")
    exApplication =
      ExApplication <$> getSourcePos <*>
        (try (exIdentifier <* symbol "(")) <*>
        (sepBy expression (symbol ",") <* symbol ")")
    exLiteral =
      ExLiteral <$> getSourcePos <*> literal
    exIdentifier =
      ExIdentifier <$> getSourcePos <*> identifier

binding :: Parser Binding
binding =
  lexeme (Binding <$> getSourcePos <*> (identifier <* symbol ":") <*> expression)

prefix :: Parser Prefix
prefix =
  lexeme
    (Prefix <$> getSourcePos <*>
      (concat <$> optional (symbol "(" *> sepBy binding (symbol ",") <* symbol ")"))
    )

statement :: Parser Statement
statement =
  lexeme (stLet) where
    stLet =
      StLet <$> getSourcePos <*>
        (symbol "let" *> identifier) <*>
        (prefix) <*>
        (symbol ":" *> expression) <*>
        (symbol "=" *> expression <* symbol "end")

program :: Parser Program
program =
  lexeme (Program <$> getSourcePos <*> some statement <* eof)
