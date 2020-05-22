module Curios.Parsing
  (literal
  ,name
  ,qualifiedName
  ,piBinding
  ,lambdaBinding
  ,abstraction
  ,expression
  ,statement
  ,program
  )
  where

import Curios.Expression
  (Literal (..)
  ,Name (..)
  ,QualifiedName (..)
  ,PiBinding (..)
  ,LambdaBinding (..)
  ,Abstraction (..)
  ,Expression (..)
  ,Statement (..)
  ,Program (..)
  )

import Data.Void
  (Void
  )

import Text.Megaparsec
  (Parsec
  ,optional
  ,try
  ,some
  ,many
  ,oneOf
  ,single
  ,someTill
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
  lexeme (liCharacter <|> liString <|> try liRational <|> liInteger) where
    liCharacter = LiCharacter <$> (single '\'' *> Lexer.charLiteral <* single '\'')
    liString = LiText <$> (single '"' *> manyTill Lexer.charLiteral (single '"'))
    liRational = raPositive <|> raNegative where
      raPositive = LiRational <$> (optional (single '+') *> Lexer.float)
      raNegative = (LiRational . negate) <$> (single '-' *> Lexer.float)
    liInteger = inPositive <|> inNegative where
      inPositive = LiInteger <$> (optional (single '+') *> Lexer.decimal)
      inNegative = (LiInteger . negate) <$> (single '-' *> Lexer.decimal)

name' :: Parser Name
name' =
  Name <$> some (oneOf naValidCharacters) where
    naValidCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ ['+', '-', '*', '/', '=', '\'']

name :: Parser Name
name =
  lexeme name'

qualifiedName :: Parser QualifiedName
qualifiedName =
  lexeme (QualifiedName <$> many (try (name' <* (single ';'))) <*> name')

piBinding :: Parser PiBinding
piBinding =
  lexeme (PiBinding <$> optional (try (name <* symbol ":")) <*> expression)

lambdaBinding :: Parser LambdaBinding
lambdaBinding =
  lexeme (LambdaBinding <$> name <*> optional (symbol ":" *> expression))

abstraction :: Parser a -> Parser (Abstraction a)
abstraction parser =
  lexeme (Abstraction <$> some (try (parser <* symbol ",")) <*> expression)

expression :: Parser Expression
expression =
  lexeme (try exLiteral <|> exVariable <|> exPiAbstraction <|> exLambdaAbstraction <|> exApplication) where
    exLiteral = ExLiteral <$> literal
    exVariable = ExVariable <$> qualifiedName
    exPiAbstraction = ExPiAbstraction <$> (symbol "[" *> abstraction piBinding <* symbol "]")
    exLambdaAbstraction = ExLambdaAbstraction <$> (symbol "{" *> abstraction lambdaBinding <* symbol "}")
    exApplication = ExApplication <$> (symbol "(" *> expression) <*> (manyTill expression (symbol ")"))

statement :: Parser Statement
statement =
  lexeme (stModule <|> stImport <|> stDefine) where
    stModule = StModule <$> (symbol "module" *> name) <*> program <* symbol "end"
    stImport = StImport <$> (symbol "import" *> someTill qualifiedName (symbol "end"))
    stDefine = StDefine <$> (symbol "define" *> name) <*> deType <*> deBody where
      deType = symbol ":" *> expression
      deBody = symbol "=" *> expression <* symbol "end"

program :: Parser Program
program =
  lexeme (Program <$> many statement)

