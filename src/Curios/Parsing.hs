module Curios.Parsing
  ( space
  , lexeme
  , symbol
  , name
  , availability
  , binding
  , quantifier
  , argument
  , literal
  , identifier
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
  , sepBy
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
  , Availability (..)
  , Binding (..)
  , Quantifier (..)
  , Argument (..)
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
      ['+', '-', '*', '/', '=', '\'']

availability :: Parser Availability
availability =
  lexeme (avImplicit <|> avExplicit) where
    avImplicit = AvImplicit <$ symbol "."
    avExplicit = AvExplicit <$ symbol "!"

quantifier :: Parser Quantifier
quantifier =
  lexeme (Quantifier <$> optional (try (name <* symbol ":")) <*> expression <*> availability)

binding :: Parser Binding
binding =
  lexeme (Binding <$> name <*> optional (try (symbol ":" *> expression)) <*> availability)

argument :: Parser Argument
argument =
  lexeme (Argument <$> expression <*> optional availability)

literal :: Parser Literal
literal =
  lexeme (liCharacter <|> liString <|> liNumber) where
    liCharacter = LiCharacter <$> (single '\'' *> Lexer.charLiteral)
    liString = LiString <$> (single '"' *> manyTill Lexer.charLiteral (single '"'))
    liNumber = try nuRational <|> nuInteger where
      nuRational = raPositive <|> raNegative where
        raPositive = LiRational <$> (optional (single '+') *> Lexer.float)
        raNegative = (LiRational . negate) <$> (single '-' *> Lexer.float)
      nuInteger = inPositive <|> inNegative where
        inPositive = LiInteger <$> (optional (single '+') *> Lexer.decimal)
        inNegative = (LiInteger . negate) <$> (single '-' *> Lexer.decimal)

identifier :: Parser Identifier
identifier =
  lexeme (Identifier <$> sepBy name (single ';'))

expression :: Parser Expression
expression =
  lexeme (exPiAbstraction <|> exLambdaAbstraction <|> exApplication <|> exLiteral <|> exVariable) where
    exPiAbstraction = ExPiAbstraction <$> (symbol "<" *> some (try quantifier)) <*> (expression <* symbol ">")
    exLambdaAbstraction = ExLambdaAbstraction <$> (symbol "{" *> some (try binding)) <*> (expression <* symbol "}")
    exApplication = ExApplication <$> (symbol "(" *> expression) <*> manyTill argument (symbol ")")
    exLiteral = ExLiteral <$> literal
    exVariable = ExVariable <$> identifier

statement :: Parser Statement
statement =
  lexeme (stPackage <|> stImport <|> stAssume <|> stDefine <|> stAlias) where
    stPackage = StPackage <$> (symbol "package" *> name) <*> (symbol "where" *> program <* symbol "end")
    stImport = StImport <$> (symbol "import" *> identifier)
    stAssume = StAssume <$> (symbol "assume" *> name) <*> (symbol ":" *> expression)
    stDefine = StDefine <$> (symbol "define" *> name) <*> (symbol ":" *> expression) <*> (symbol "=" *> expression)
    stAlias = StAlias <$> (symbol "alias" *> name) <*> (symbol "=" *> expression)

program :: Parser Program
program =
  lexeme (Program <$> many (try statement))