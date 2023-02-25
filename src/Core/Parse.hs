module Core.Parse
  ( parse
  )
  where

import Core.Syntax
  ( wrap
  , unbound
  , abstract
  , commit
  , BinOp (..)
  , BoolOp (..)
  , CompOp (..)
  , Term (..)
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
import Core.Program (Item (..), Program (..))
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Megaparsec.Char (space1)
import Control.Monad (when)
import Data.List (intercalate)

type Parse = Parsec String String

runParse :: Parse a -> String -> Either (ParseErrorBundle String String) a
runParse action = runParser action ""

parseSpace :: Parse ()
parseSpace = space space1 (skipLineComment "//") (skipBlockComment "/*" "*/")

parseLexeme :: Parse a -> Parse a
parseLexeme = lexeme parseSpace

parseSymbol :: String -> Parse String
parseSymbol = symbol parseSpace

parseIdentifier :: Parse String
parseIdentifier = parseLexeme (some $ oneOf validCharacters) where
  validCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']

parseParens :: Parse Term
parseParens = between (parseSymbol "(") (parseSymbol ")") parseTerm

parseOrigin :: Parse Origin
parseOrigin = Source <$> getSourcePos

parseInt32 :: Parse Term
parseInt32 = parseLexeme (positive <|> negative) where
  positive = Int32 <$> parseOrigin <*> (optional (single '+') *> decimal)
  negative = Int32 <$> parseOrigin <*> (single '-' *> (negate <$> decimal))

parseFlt32 :: Parse Term
parseFlt32 = parseLexeme (positive <|> negative) where
  positive = Flt32 <$> parseOrigin <*> (optional (single '+') *> float)
  negative = Flt32 <$> parseOrigin <*> (single '-' *> (negate <$> float))

pairs :: Origin -> [Term] -> Term
pairs origin = \case
  [] -> error "can't build a pair with 0 entries"
  [_] -> error "can't build a pair with 1 entry"
  [left, right] -> Pair origin left right
  term : terms -> Pair origin term (pairs origin terms)

parsePair :: Parse Term
parsePair = do
  origin <- parseOrigin

  terms <- between
    (parseSymbol "(")
    (parseSymbol ")")
    (sepBy parseTerm (parseSymbol ","))

  when (length terms < 2)
    (customFailure "Pairs need at least 2 entries")

  return (pairs origin terms)

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
  body <- parseSymbol "=" *> parseTerm <* parseSymbol ";"
  return (label, body)

parseMatch :: Parse Term
parseMatch = do
  origin <- parseOrigin
  scrutinee <- parseSymbol "match " *> parseTerm <* parseSymbol "{"
  branches <- many parseBranch <* parseSymbol "}"
  return (Match origin scrutinee branches)

parseType :: Parse Term
parseType = Type <$> parseOrigin <* parseSymbol "Type"

parseInt32Type :: Parse Term
parseInt32Type = Int32Type <$> parseOrigin <* parseSymbol "Int32"

parseFlt32Type :: Parse Term
parseFlt32Type = Flt32Type <$> parseOrigin <* parseSymbol "Flt32"

parseInt32If :: Parse Term
parseInt32If = do
  origin <- parseOrigin
  scrutinee <- parseSymbol "if " *> parseClosed
  Int32If origin scrutinee <$> parseClosed <*> parseClosed

parseInt32BinOp :: Parse Term
parseInt32BinOp = do
  origin <- parseOrigin

  op <- Add <$ parseSymbol "+i"
    <|> Sub <$ parseSymbol "-i"
    <|> Mul <$ parseSymbol "*i"
    <|> Div <$ parseSymbol "/i"

  Int32BinOp origin op <$> parseClosed <*> parseClosed

parseInt32BoolOp :: Parse Term
parseInt32BoolOp = do
  origin <- parseOrigin

  op <- And <$ parseSymbol "&&i"
    <|> Or <$ parseSymbol "||i"

  Int32BoolOp origin op <$> parseClosed <*> parseClosed

parseInt32CompOp :: Parse Term
parseInt32CompOp = do
  origin <- parseOrigin

  op <- Eq <$ parseSymbol "==i"
    <|> Ne <$ parseSymbol "/=i"
    <|> try (Lt <$ parseSymbol "<i")
    <|> Le <$ parseSymbol "<=i"
    <|> try (Gt <$ parseSymbol ">i")
    <|> Ge <$ parseSymbol ">=i"

  Int32CompOp origin op <$> parseClosed <*> parseClosed

parseFlt32BinOp :: Parse Term
parseFlt32BinOp = do
  origin <- parseOrigin

  op <- Add <$ parseSymbol "+f"
    <|> Sub <$ parseSymbol "-f"
    <|> Mul <$ parseSymbol "*f"
    <|> Div <$ parseSymbol "/f"

  Flt32BinOp origin op <$> parseClosed <*> parseClosed

parseFlt32CompOp :: Parse Term
parseFlt32CompOp = do
  origin <- parseOrigin

  op <- Eq <$ parseSymbol "==f"
    <|> Ne <$ parseSymbol "/=f"
    <|> try (Lt <$ parseSymbol "<f")
    <|> Le <$ parseSymbol "<=f"
    <|> try (Gt <$ parseSymbol ">f")
    <|> Ge <$ parseSymbol ">=f"

  Flt32CompOp origin op <$> parseClosed <*> parseClosed

parseName :: Parse Term
parseName = Local <$> parseOrigin <*> (wrap <$> parseIdentifier)

parseClosed :: Parse Term
parseClosed = try parseParens
  <|> try parsePair
  <|> try parseLabelType
  <|> try parseLabel
  <|> try parseInt32
  <|> try parseFlt32
  <|> try parseMatch
  <|> try parseType
  <|> try parseInt32Type
  <|> try parseFlt32Type
  <|> try parseInt32If
  <|> try parseInt32BinOp
  <|> try parseInt32BoolOp
  <|> try parseInt32CompOp
  <|> try parseFlt32BinOp
  <|> try parseFlt32CompOp
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
      input <- parseSymbol ":" *> parseTerm <* parseSymbol ")" <* parseSymbol "*"
      scope <- abstract identifier <$> parseTerm
      return (PairType origin input scope)

    parseNonDependent = do
      input <- parseApply <* parseSymbol "*"
      scope <- unbound <$> parseTerm
      return (PairType origin input scope)

  try parseDependent <|> parseNonDependent

splits :: Origin -> [String] -> Term -> Term -> Term
splits origin names scrutinee body = case names of
  [] -> error "can't build split expression with 0 entries"
  [_] -> error "can't build split expression with 1 entry"

  [left, right] -> Split origin scrutinee scope where
    scope = abstract left (abstract right body)

  name : rest -> Split origin scrutinee scope where
    scrutineeName = intercalate ", " rest
    scrutineeTerm = Local Machine (wrap scrutineeName)
    term = splits origin rest scrutineeTerm body
    scope = abstract name (abstract scrutineeName term)

parseSplit :: Parse Term
parseSplit = do
  origin <- parseOrigin

  names <- parseSymbol "let" *> between
    (parseSymbol "(")
    (parseSymbol ")")
    (sepBy parseIdentifier (parseSymbol ","))

  when (length names < 2)
    (customFailure "Split expressions need at least 2 entries")

  scrutinee <- parseSymbol "=" *> parseTerm <* parseSymbol ";"

  splits origin names scrutinee <$> parseTerm

parseFunctionType :: Parse Term
parseFunctionType = do
  origin <- parseOrigin

  let
    parseDependent = do
      identifier <- parseSymbol "(" *> parseIdentifier
      input <- parseSymbol ":" *> parseTerm <* parseSymbol ")" <* parseSymbol "->"
      scope <- abstract identifier <$> parseTerm
      return (FunctionType origin input scope)

    parseNonDependent = do
      input <- parseApply <* parseSymbol "->"
      scope <- unbound <$> parseTerm
      return (FunctionType origin input scope)

  try parseDependent <|> parseNonDependent

parseFunction :: Parse Term
parseFunction = do
  origin <- parseOrigin
  identifier <- parseIdentifier <* parseSymbol "=>"
  body <- abstract identifier <$> parseTerm
  return (Function origin body)

parseTerm :: Parse Term
parseTerm = try parseFunctionType
  <|> try parsePairType
  <|> try parseFunction
  <|> try parseSplit
  <|> parseApply

parseItem :: Parse Item
parseItem = do
  let
    parseDeclaration = do
      sourcePos <- getSourcePos
      identifier <- parseIdentifier <* parseSymbol ":"
      declaration <- parseTerm <* parseSymbol ";"
      return (Declaration sourcePos identifier $ commit declaration)

    parseDefinition = do
      sourcePos <- getSourcePos
      identifier <- parseIdentifier <* parseSymbol "="
      definition <- parseTerm <* parseSymbol ";"
      return (Definition sourcePos identifier $ commit definition)

  try parseDeclaration <|> parseDefinition

parseProgram :: Parse Program
parseProgram = Program <$> someTill parseItem eof

parse :: String -> Either Error Program
parse source = case runParse parseProgram source of
  Left bundle -> Left (fromParseErrorBundle bundle)
  Right program -> Right program
