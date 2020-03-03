module Curios.Parsing
  ( name
  , qualifiedName
  , atom
  , piBinding
  , lambdaBinding
  , abstraction
  , expression
  , statement
  , program
  )
  where

import Data.Void
  ( Void
  )

import Text.Megaparsec
  ( Parsec
  , optional
  , try
  , some
  , many
  , oneOf
  , single
  , manyTill
  , sepBy1
  , (<|>)
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
  ( Name
  , QualifiedName
  , Atom (..)
  , PiBinding (..)
  , LambdaBinding (..)
  , Abstraction (..)
  , Expression (..)
  , Statement (..)
  , Program
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
  lexeme (some (oneOf naValidCharacters)) where
    naValidCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ ['+', '-', '*', '/', '=']

qualifiedName :: Parser QualifiedName
qualifiedName =
  lexeme (sepBy1 name (single ';'))

piBinding :: Parser PiBinding
piBinding =
  lexeme (PiBinding <$> optional (try (name <* symbol ":")) <*> expression)

lambdaBinding :: Parser LambdaBinding
lambdaBinding =
  lexeme (LambdaBinding <$> name <*> optional (symbol ":" *> expression))

abstraction :: Parser a -> Parser (Abstraction a)
abstraction parser =
  lexeme (Abstraction <$> some (try (parser <* symbol ".")) <*> expression)

atom :: Parser Atom
atom =
  lexeme (atCharacter <|> atString <|> try atRational <|> atInteger <|> atSymbol) where
    atCharacter = AtCharacter <$> (single '\'' *> Lexer.charLiteral)
    atString = AtString <$> (single '"' *> manyTill Lexer.charLiteral (single '"'))
    atRational = raPositive <|> raNegative where
      raPositive = AtRational <$> (optional (single '+') *> Lexer.float)
      raNegative = (AtRational . negate) <$> (single '-' *> Lexer.float)
    atInteger = inPositive <|> inNegative where
      inPositive = AtInteger <$> (optional (single '+') *> Lexer.decimal)
      inNegative = (AtInteger . negate) <$> (single '-' *> Lexer.decimal)
    atSymbol = AtSymbol <$> qualifiedName

expression :: Parser Expression
expression =
  lexeme (exAtom <|> exPiAbstraction <|> exLambdaAbstraction <|> exApplication) where
    exAtom = ExAtom <$> atom
    exPiAbstraction = ExPiAbstraction <$> (symbol "<" *> abstraction piBinding <* symbol ">")
    exLambdaAbstraction = ExLambdaAbstraction <$> (symbol "{" *> abstraction lambdaBinding <* symbol "}")
    exApplication = ExApplication <$> (symbol "[" *> expression) <*> (manyTill expression (symbol "]"))

statement :: Parser Statement
statement =
  lexeme (symbol "(" *> (stModule <|> stImport <|> stDefine) <* symbol ")") where
    stModule = StModule <$> (symbol "module" *> name) <*> program
    stImport = StImport <$> (symbol "import" *> qualifiedName)
    stDefine = StDefine <$> (symbol "define" *> name) <*> expression <*> expression

program :: Parser Program
program =
  lexeme (many statement)

