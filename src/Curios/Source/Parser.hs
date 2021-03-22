module Curios.Source.Parser
  (program
  )
  where

import Text.Megaparsec.Char (space1)
import Data.Void (Void)

import Curios.Source.Types
  (Identifier (..)
  ,Literal (..)
  ,FunctionTypeBinding (..)
  ,FunctionBinding (..)
  ,Expression (..)
  ,Binding (..)
  ,Prefix (..)
  ,Statement (..)
  ,Program (..)
  )

import Text.Megaparsec
  (Parsec
  ,getSourcePos
  ,try
  ,oneOf
  ,single
  ,optional
  ,many
  ,some
  ,manyTill
  ,someTill
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
    validCharacters =
      ['a' .. 'z']
        ++ ['A' .. 'Z']
        ++ ['0' .. '9']
        ++ ['+', '-', '*', '/', '=', '>', '<', '\'', '_', '.', '~']

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

functionTypeBinding :: Parser FunctionTypeBinding
functionTypeBinding =
  lexeme (try named <|> unnamed) where
    named =
      FunctionTypeBinding <$> getSourcePos
        <*> (symbol "(" *> optional (try (identifier <* symbol "|")))
        <*> optional (try (identifier <* symbol ":"))
        <*> exFunctionType (symbol ")") <* symbol "->"
    unnamed = 
      FunctionTypeBinding <$> getSourcePos
        <*> pure Nothing
        <*> pure Nothing
        <*> exApplication (symbol "->")

functionBinding :: Parser FunctionBinding
functionBinding =
  lexeme (FunctionBinding <$> getSourcePos <*> (identifier <* symbol "=>"))

exClosed :: Parser Expression
exClosed =
  lexeme (try exLiteral <|> exIdentifier <|> exParens) where
    exLiteral = ExLiteral <$> getSourcePos <*> literal
    exIdentifier = ExIdentifier <$> getSourcePos <*> identifier
    exParens = ExParens <$> getSourcePos <*> (symbol "(" *> exFunction (symbol ")"))

exApplication :: Parser a -> Parser Expression
exApplication terminator =
  lexeme (try application <|> exClosed <* terminator) where
    application =
      ExApplication <$> getSourcePos
        <*> exClosed
        <*> someTill exClosed terminator

exFunctionType :: Parser a -> Parser Expression
exFunctionType terminator =
  lexeme (functionType <|> exApplication terminator) where
    functionType =
      ExFunctionType <$> getSourcePos
        <*> some (try functionTypeBinding)
        <*> exApplication terminator

exFunction :: Parser a -> Parser Expression
exFunction terminator =
  lexeme (function <|> exFunctionType terminator) where
    function =
      ExFunction <$> getSourcePos
        <*> some (try functionBinding)
        <*> exFunctionType terminator

expression :: Parser a -> Parser Expression
expression terminator =
  lexeme (exFunction terminator)

binding :: Parser Binding
binding =
  Binding <$> getSourcePos <*> (symbol "(" *> identifier <* symbol ":") <*> expression (symbol ")")

prefix :: Parser Prefix
prefix =
  lexeme (Prefix <$> getSourcePos <*> bindings) where
    bindings = concat <$> optional (many binding)

statement :: Parser Statement
statement =
  lexeme (Statement <$> getSourcePos <*> stIdentifier <*> prefix <*> stDeclaration <*> stDefinition) where
    stIdentifier = symbol "let" *> identifier
    stDeclaration = symbol ":" *> expression (symbol "{")
    stDefinition = expression (symbol "}")

program :: Parser Program
program =
  lexeme (Program <$> getSourcePos <*> some statement <* eof)
