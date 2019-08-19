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
import Control.Applicative

import qualified Text.Megaparsec as Me
import qualified Text.Megaparsec.Char as Ch
import qualified Text.Megaparsec.Char.Lexer as Le

import Curios.Expression
import Curios.Program

type Parser a =
  Me.Parsec Void String a

space :: Parser ()
space =
  Le.space Ch.space1 spLine spBlock where
    spLine = Le.skipLineComment "//"
    spBlock = Le.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme parser =
  Le.lexeme space parser

symbol :: String -> Parser String
symbol string =
  Le.symbol space string

name :: Parser String
name =
  lexeme (Me.some (Me.oneOf naValidCharacters)) where
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
  lexeme (Quantifier <$> Me.optional (name <* symbol ":") <*> expression <*> availability)

binding :: Parser Binding
binding =
  lexeme (Binding <$> name <*> Me.optional (symbol ":" *> expression) <*> availability)

argument :: Parser Argument
argument =
  lexeme (Argument <$> expression <*> Me.optional availability)

literal :: Parser Literal
literal =
  lexeme (liCharacter <|> liString <|> liNumber) where
    liCharacter = LiCharacter <$> (Me.single '\'' *> Le.charLiteral)
    liString = LiString <$> (Me.single '"' *> Me.manyTill Le.charLiteral (Me.single '"'))
    liNumber = Me.try nuRational <|> nuInteger where
      nuRational = raPositive <|> raNegative where
        raPositive = LiRational <$> (optional (Me.single '+') *> Le.float)
        raNegative = (LiRational . negate) <$> (Me.single '-' *> Le.float)
      nuInteger = inPositive <|> inNegative where
        inPositive = LiInteger <$> (optional (Me.single '+') *> Le.decimal)
        inNegative = (LiInteger . negate) <$> (Me.single '-' *> Le.decimal)

identifier :: Parser Identifier
identifier =
  lexeme (Identifier <$> Me.sepBy name (Me.single ';'))

expression :: Parser Expression
expression =
  lexeme (exPiAbstraction <|> exLambdaAbstraction <|> exFunctionApplication <|> exLiteral <|> exVariable) where
    exPiAbstraction = ExPiAbstraction <$> (symbol "<" *> Me.some quantifier) <*> (expression <* symbol ">")
    exLambdaAbstraction = ExLambdaAbstraction <$> (symbol "{" *> Me.some binding) <*> (expression <* symbol "}")
    exFunctionApplication = ExApplication <$> (symbol "(" *> expression) <*> Me.manyTill argument (symbol ")")
    exLiteral = ExLiteral <$> literal
    exVariable = ExVariable <$> identifier

statement :: Parser Statement
statement =
  lexeme (stModule <|> stImport <|> stAssume <|> stDefine) where
    stModule = StProgram <$> program
    stImport = StImport <$> (symbol "import" *> identifier)
    stAssume = StAssume <$> (symbol "assume" *> name) <*> (symbol ":" *> expression)
    stDefine = StDefine <$> (symbol "define" *> name) <*> (symbol ":" *> expression) <*> (symbol "=" *> expression)

program :: Parser Program
program =
  lexeme (Program <$> (symbol "module" *> name) <*> (symbol "where" *> Me.manyTill statement (symbol "end")))
