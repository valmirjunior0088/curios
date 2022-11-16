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
  , some
  , many
  , someTill
  , manyTill
  , try
  , oneOf
  , (<|>)
  , eof
  , single
  , optional
  , sepBy
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
import Data.Void (Void)
import Data.Functor ((<&>))
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Megaparsec.Char (space1)
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT, asks)

type Parse = ReaderT [String] (Parsec Void String)

runParse :: Parse a -> String -> Either (ParseErrorBundle String Void) a
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

parsePair :: Parse Term
parsePair = do
  origin <- parseOrigin
  left <- parseSymbol "(" *> parseTerm ","
  terms <- many (try $ parseTerm ",")
  right <- parseTerm ")"
  return (Pair origin left $ foldr (Pair origin) right terms)

parseLabelType :: Parse Term
parseLabelType = do
  origin <- parseOrigin

  labels <- parseSymbol "{"
    *> sepBy (single ':' *> parseIdentifier) (parseSymbol ",")
    <* parseSymbol "}"

  return (LabelType origin labels)

parseLabel :: Parse Term
parseLabel = do
  origin <- parseOrigin
  label <- single ':' *> parseIdentifier
  return (Label origin label)

parseBranch :: Parse (String, Term)
parseBranch = do
  label <- single ':' *> parseIdentifier
  body <- parseSymbol "=" *> parseTerm "."
  return (label, body)

parseMatch :: Parse Term
parseMatch = do
  origin <- parseOrigin
  scrutinee <- parseSymbol "match " *> parseTerm "{"
  branches <- manyTill parseBranch (parseSymbol "}")
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
parseParens = parseSymbol "(" *> parseTerm ")"

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
parseClosed = try parsePair
  <|> try parseLabelType
  <|> try parseLabel
  <|> try parseMatch
  <|> try parsePrimitive
  <|> try parseOperate
  <|> try parseParens
  <|> parseName

parseApply :: String -> Parse Term
parseApply boundary = do
  origin <- parseOrigin
  terms <- someTill parseClosed (parseSymbol boundary)
  return (foldl1 (Apply origin) terms)

parsePairType :: String -> Parse Term
parsePairType boundary = do
  origin <- parseOrigin

  let
    parseDependent = do
      identifier <- parseSymbol "(" *> parseIdentifier
      input <- parseSymbol ":" *> parseTerm ")" <* parseSymbol "*>"
      scope <- parseScope identifier (parseTerm boundary)
      return (PairType origin input scope)

    parseNonDependent = do
      input <- parseApply "*>"
      scope <- parseUnboundScope (parseTerm boundary)
      return (PairType origin input scope)

  try parseDependent <|> parseNonDependent

parseSplit :: String -> Parse Term
parseSplit boundary = do
  origin <- parseOrigin
  left <- parseSymbol "let" *> parseSymbol "(" *> parseIdentifier
  right <- parseSymbol "," *> parseIdentifier
  scrutinee <- parseSymbol ")" *> parseSymbol "=" *> parseTerm ";"
  body <- parseScope left (parseScope right (parseTerm boundary))
  return (Split origin scrutinee body)

parseFunctionType :: String -> Parse Term
parseFunctionType boundary = do
  origin <- parseOrigin

  let
    parseDependent = do
      identifier <- parseSymbol "(" *> parseIdentifier
      input <- parseSymbol ":" *> parseTerm ")" <* parseSymbol "->"
      scope <- parseScope identifier (parseTerm boundary)
      return (FunctionType origin input scope)

    parseNonDependent = do
      input <- parseApply "->"
      scope <- parseUnboundScope (parseTerm boundary)
      return (FunctionType origin input scope)

  try parseDependent <|> parseNonDependent

parseFunction :: String -> Parse Term
parseFunction boundary = do
  origin <- parseOrigin
  identifier <- parseIdentifier <* parseSymbol "=>"
  body <- parseScope identifier (parseTerm boundary)
  return (Function origin body)

parseTerm :: String -> Parse Term
parseTerm boundary = try (parseFunction boundary)
  <|> try (parseFunctionType boundary)
  <|> try (parseSplit boundary)
  <|> try (parsePairType boundary)
  <|> parseApply boundary

parseEntry :: Parse Entry
parseEntry = try parseDeclaration <|> parseDefinition where
  parseDeclaration = do
    sourcePos <- getSourcePos
    identifier <- parseIdentifier <* parseSymbol ":"
    declaration <- parseTerm "."
    return (Declaration sourcePos identifier declaration)

  parseDefinition = do
    sourcePos <- getSourcePos
    identifier <- parseIdentifier <* parseSymbol "="
    definition <- parseTerm "."
    return (Definition sourcePos identifier definition)

parseProgram :: Parse Program
parseProgram = Program <$> someTill parseEntry eof

parse :: String -> Either Error Program
parse source = case runParse parseProgram source of
  Left bundle -> Left (fromParseErrorBundle bundle)
  Right program -> Right program
