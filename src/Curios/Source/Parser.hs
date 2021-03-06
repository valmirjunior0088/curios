module Curios.Source.Parser
  ( parse
  )
  where

import Curios.Source.Error (Error, erFromMegaparsec)
import Text.Megaparsec.Char (space1)
import Data.Void (Void)

import qualified Text.Megaparsec as Megaparsec

import Curios.Source
  ( Identifier (..)
  , Literal (..)
  , FunctionTypeBinding (..)
  , FunctionBinding (..)
  , Expression (..)
  , Binding (..)
  , Statement (..)
  , Program (..)
  )

import Text.Megaparsec
  ( Parsec
  , getSourcePos
  , try
  , oneOf
  , single
  , optional
  , many
  , some
  , manyTill
  , someTill
  , eof
  , (<|>)
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
        ++ ['+', '-', '*', '/', '=', '>', '<', '\'', '_', '~']

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
  lexeme (withParens <|> withoutParens) where
    withParens =
      FunctionTypeBinding <$> getSourcePos
        <*> (symbol "(" *> optional (try (identifier <* symbol "|")))
        <*> optional (try (identifier <* symbol ":"))
        <*> exFunction (symbol ")") <* symbol "->"
    withoutParens = 
      FunctionTypeBinding <$> getSourcePos
        <*> pure Nothing
        <*> pure Nothing
        <*> exApplication (symbol "->")

functionBinding :: Parser FunctionBinding
functionBinding =
  lexeme (FunctionBinding <$> getSourcePos <*> identifier <* symbol "=>")

exClosed :: Parser Expression
exClosed =
  lexeme (try exLiteral <|> exIdentifier <|> exParens) where
    exLiteral = ExLiteral <$> getSourcePos <*> literal
    exIdentifier = ExIdentifier <$> getSourcePos <*> identifier
    exParens = ExParens <$> getSourcePos <*> (symbol "(" *> exFunction (symbol ")"))

exApplication :: Parser a -> Parser Expression
exApplication terminator = do
  sourcePos <- getSourcePos
  expressions <- someTill exClosed terminator

  case expressions of
    [] -> error "empty application"
    closed : [] -> return closed
    function : arguments -> return (ExApplication sourcePos function arguments)

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

binding :: Parser Binding
binding =
  lexeme (Binding <$> getSourcePos <*> name <*> declaration) where
    name = symbol "(" *> identifier
    declaration = symbol ":" *> exFunction (symbol ")")

statement :: Parser Statement
statement =
  StDefn <$> getSourcePos
    <*> (symbol "defn" *> identifier)
    <*> many binding
    <*> (symbol ":" *> exFunction (symbol "{"))
    <*> exFunction (symbol "}")

program :: Parser Program
program =
  lexeme (Program <$> getSourcePos <*> some statement <* eof)

parse :: String -> String -> Either Error Program
parse file source =
  case Megaparsec.parse program file source of
    Left parseErrorBundle -> Left (erFromMegaparsec parseErrorBundle)
    Right result -> Right result
