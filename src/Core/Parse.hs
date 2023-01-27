module Core.Parse
  ( parse
  )
  where

import Core.Syntax
  ( Variable (..)
  , Scope
  , unbound
  , PrimitiveType (..)
  , Primitive (..)
  , Operation (..)
  , Term (..)
  , Walk
  , abstract
  )

import Text.Megaparsec
  ( Parsec
  , runParser
  , customFailure
  , some
  , someTill
  , many
  , try
  , oneOf
  , (<|>)
  , eof
  , single
  , optional
  , sepBy
  , between
  , getSourcePos
  )

import Text.Megaparsec.Char.Lexer
  ( space
  , skipLineComment
  , skipBlockComment
  , lexeme
  , symbol
  , decimal
  , float
  )

import Error (Origin (..), Error, fromParseErrorBundle)
import Core.Program (Entry (..), Program (..))
import Data.Functor ((<&>))
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Megaparsec.Char (space1)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT, asks)

type Parse = ReaderT [String] (Parsec String String)

runParse :: Parse a -> String -> Either (ParseErrorBundle String String) a
runParse action = runParser (runReaderT action []) ""

parseSpace :: Parse ()
parseSpace = space space1 (skipLineComment "//") (skipBlockComment "/*" "*/")

parseLexeme :: Parse a -> Parse a
parseLexeme = lexeme parseSpace

parseSymbol :: String -> Parse String
parseSymbol = symbol parseSpace

parseIdentifier :: Parse String
parseIdentifier = parseLexeme (some $ try $ oneOf validCharacters) where
  validCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']

parseOrigin :: Parse Origin
parseOrigin = Source <$> getSourcePos

parseScope :: Walk a => String -> Parse a -> Parse (Scope a)
parseScope identifier parser = do
  scope <- local (identifier :) parser
  return (abstract identifier scope)

parseUnboundScope :: Parse a -> Parse (Scope a)
parseUnboundScope parser = unbound <$> parser

buildNestedPairs :: Origin -> [Term] -> Term
buildNestedPairs origin = \case
  [] -> error "can't build a pair with 0 entries"
  [_] -> error "can't build a pair with 1 entry"
  [left, right] -> Pair origin left right
  term : terms -> Pair origin term (buildNestedPairs origin terms)

parsePair :: Parse Term
parsePair = do
  origin <- parseOrigin

  terms <- between
    (parseSymbol "(")
    (parseSymbol ")")
    (sepBy parseTerm (parseSymbol ","))

  when (length terms < 2)
    (customFailure "Pairs need at least 2 entries")

  return (buildNestedPairs origin terms)

parseLabelType :: Parse Term
parseLabelType = do
  origin <- parseOrigin

  labels <- between
    (parseSymbol "{")
    (parseSymbol "}")
    (sepBy (single ':' *> parseIdentifier) (parseSymbol ","))

  return (LabelType origin labels)

parseLabel :: Parse Term
parseLabel = do
  origin <- parseOrigin
  label <- single ':' *> parseIdentifier
  return (Label origin label)

parseBranch :: Parse (String, Term)
parseBranch = do
  label <- single ':' *> parseIdentifier
  body <- parseSymbol "=" *> parseTerm <* parseSymbol "."
  return (label, body)

parseMatch :: Parse Term
parseMatch = do
  origin <- parseOrigin
  scrutinee <- parseSymbol "match " *> parseTerm <* parseSymbol "{"
  branches <- many parseBranch <* parseSymbol "}"
  return (Match origin scrutinee branches)

parseInt :: Parse Primitive
parseInt = parseLexeme (positive <|> negative) where
  positive = Int32 <$> (optional (single '+') *> decimal)
  negative = Int32 <$> (single '-' *> (negate <$> decimal))

parseFloat :: Parse Primitive
parseFloat = parseLexeme (positive <|> negative) where
  positive = Flt32 <$> (optional (single '+') *> float)
  negative = Flt32 <$> (single '-' *> (negate <$> float))

parsePrimitive :: Parse Term
parsePrimitive = do
  origin <- parseOrigin
  primitive <- parseInt <|> parseFloat
  return (Primitive origin primitive)

parseOperation :: Parse Operation
parseOperation =
  Int32Add <$ parseSymbol "int32.add " <|> Flt32Add <$ parseSymbol "flt32.add "

parseOperate :: Parse Term
parseOperate = do
  origin <- parseOrigin
  operation <- try (parseSymbol "[" *> parseOperation)
  parameters <- someTill parseClosed (parseSymbol "]")
  return (Operate origin operation parameters)

parseParens :: Parse Term
parseParens = between (parseSymbol "(") (parseSymbol ")") parseTerm

parseName :: Parse Term
parseName = do
  origin <- parseOrigin

  parseIdentifier >>= \case
    "Type" -> return (Type origin)
    "Int32" -> return (PrimitiveType origin Int32Type)
    "Flt32" -> return (PrimitiveType origin Flt32Type)

    identifier -> asks (elem identifier) <&> \case
      True -> Local origin (Free identifier)
      False -> Global origin identifier

parseClosed :: Parse Term
parseClosed = try parseParens
  <|> try parsePair
  <|> try parseLabelType
  <|> try parseLabel
  <|> try parseMatch
  <|> try parsePrimitive
  <|> try parseOperate
  <|> parseName

parseApply :: Parse Term
parseApply = do
  origin <- parseOrigin
  terms <- some parseClosed
  return (foldl1 (Apply origin) terms)

parsePairType :: Parse Term
parsePairType = do
  origin <- parseOrigin

  let
    parseDependent = do
      identifier <- parseSymbol "(" *> parseIdentifier
      input <- parseSymbol ":" *> parseTerm <* parseSymbol ")" <* parseSymbol "*>"
      scope <- parseScope identifier parseTerm
      return (PairType origin input scope)

    parseNonDependent = do
      input <- parseApply <* parseSymbol "*>"
      scope <- parseUnboundScope parseTerm
      return (PairType origin input scope)

  try parseDependent <|> parseNonDependent

parseSplit :: Parse Term
parseSplit = do
  origin <- parseOrigin
  left <- parseSymbol "let" *> parseSymbol "(" *> parseIdentifier
  right <- parseSymbol "," *> parseIdentifier
  scrutinee <- parseSymbol ")" *> parseSymbol "=" *> parseTerm <* parseSymbol ";"
  body <- parseScope left (parseScope right parseTerm)
  return (Split origin scrutinee body)

parseFunctionType :: Parse Term
parseFunctionType = do
  origin <- parseOrigin

  let
    parseDependent = do
      identifier <- parseSymbol "(" *> parseIdentifier
      input <- parseSymbol ":" *> parseTerm <* parseSymbol ")" <* parseSymbol "->"
      scope <- parseScope identifier parseTerm
      return (FunctionType origin input scope)

    parseNonDependent = do
      input <- parseApply <* parseSymbol "->"
      scope <- parseUnboundScope parseTerm
      return (FunctionType origin input scope)

  try parseDependent <|> parseNonDependent

parseFunction :: Parse Term
parseFunction = do
  origin <- parseOrigin
  identifier <- parseIdentifier <* parseSymbol "=>"
  body <- parseScope identifier parseTerm
  return (Function origin body)

parseTerm :: Parse Term
parseTerm = try parseFunction
  <|> try parseFunctionType
  <|> try parseSplit
  <|> try parsePairType
  <|> parseApply

parseEntry :: Parse Entry
parseEntry = do
  let
    parseDeclaration = do
      sourcePos <- getSourcePos
      identifier <- parseIdentifier <* parseSymbol ":"
      declaration <- parseTerm <* parseSymbol "."
      return (Declaration sourcePos identifier declaration)

    parseDefinition = do
      sourcePos <- getSourcePos
      identifier <- parseIdentifier <* parseSymbol "="
      definition <- parseTerm <* parseSymbol "."
      return (Definition sourcePos identifier definition)

  try parseDeclaration <|> parseDefinition

parseProgram :: Parse Program
parseProgram = Program <$> someTill parseEntry eof

parse :: String -> Either Error Program
parse source = case runParse parseProgram source of
  Left bundle -> Left (fromParseErrorBundle bundle)
  Right program -> Right program
