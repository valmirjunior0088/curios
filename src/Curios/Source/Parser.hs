module Curios.Source.Parser
  ( parse
  )
  where

import Curios.Source.Error (Error (..), fromErrorBundle)
import Data.Void (Void)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Lexer

import Curios.Source.Expression
  ( Identifier (..)
  , FunctionTypeBinding (..)
  , FunctionBinding (..)
  , SelfBinding (..)
  , Binding (..)
  , Primitive (..)
  , Literal (..)
  , Operation (..)
  , Expression (..)
  , Item (..)
  , Items (..)
  )

import Text.Megaparsec
  ( Parsec
  , getSourcePos
  , (<|>)
  , try
  , some
  , many
  , someTill
  , oneOf
  , optional
  , single
  , eof
  )

type Parser =
  Parsec Void String

psSpace :: Parser ()
psSpace =
  Lexer.space space1 spLineComment spBlockComment where
    spLineComment = Lexer.skipLineComment "//"
    spBlockComment = Lexer.skipBlockComment "/*" "*/"

psLexeme :: Parser a -> Parser a
psLexeme parser =
  Lexer.lexeme psSpace parser

psSymbol :: String -> Parser String
psSymbol string =
  Lexer.symbol psSpace string

psIdentifier :: Parser Identifier
psIdentifier =
  psLexeme $ Identifier <$> getSourcePos <*> string where
    validCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
    string = some $ try $ oneOf validCharacters

psFunctionTypeBinding :: Parser FunctionTypeBinding 
psFunctionTypeBinding =
  psLexeme $ try dependent <|> nonDependent where
    dependent =
      FtDependent <$> getSourcePos
        <*> (psSymbol "(" *> psIdentifier <* psSymbol ":")
        <*> psExpression ")" <* psSymbol "->"
    
    nonDependent =
      FtNonDependent <$> getSourcePos
        <*> psApplication "->"

psFunctionBinding :: Parser FunctionBinding
psFunctionBinding =
  psLexeme $ FunctionBinding <$> getSourcePos
    <*> psIdentifier <* psSymbol "=>"

psSelfBinding :: Parser SelfBinding
psSelfBinding =
  psLexeme $ SelfBinding <$> getSourcePos
    <*> psIdentifier <* psSymbol "@>"

psBinding :: Parser Binding
psBinding =
  psLexeme $ Binding <$> getSourcePos
    <*> (psSymbol "(" *> psIdentifier <* psSymbol ":")
    <*> psExpression ")"

psPrimitive :: Parser Primitive
psPrimitive =
  psLexeme $ try ltInt32 <|> ltFlt32 where
    ltInt32 = PrInt32 <$> getSourcePos <* psSymbol "Int32"
    ltFlt32 = PrFlt32 <$> getSourcePos <* psSymbol "Flt32"

psLiteral :: Parser Literal
psLiteral =
  psLexeme $ try ltReal <|> ltInteger where
    ltInteger = positive <|> negative where
      parser = LtInt32 <$> getSourcePos
      positive = parser <*> (optional (single '+') *> Lexer.decimal)
      negative = parser <*> (single '-' *> (negate <$> Lexer.decimal))
    ltReal = positive <|> negative where
      parser = LtFlt32 <$> getSourcePos
      positive = parser <*> (optional (single '+') *> Lexer.float)
      negative = parser <*> (single '-' *> (negate <$> Lexer.float))

psOperation :: Parser Operation
psOperation =
  psLexeme parser where
    opInt32Sum = OpInt32Sum <$> getSourcePos <* psSymbol "int32_sum "
    opFlt32Sum = OpFlt32Sum <$> getSourcePos <* psSymbol "flt32_sum "
    
    parser = try opInt32Sum <|> opFlt32Sum

psClosed :: Parser Expression
psClosed =
  psLexeme parser where
    exParentheses =
      ExParentheses <$> getSourcePos
        <*> (psSymbol "(" *> psExpression ")")

    exPrimitive =
      ExPrimitive <$> getSourcePos
        <*> psPrimitive

    exLiteral =
      ExLiteral <$> getSourcePos
        <*> psLiteral

    exOperation =
      ExOperation <$> getSourcePos
        <*> (psSymbol "#[" *> psOperation)
        <*> (someTill psClosed $ psSymbol "]")

    exType =
      ExType <$> getSourcePos
        <* psSymbol "Type"

    exIdentifier =
      ExIdentifier <$> getSourcePos
        <*> psIdentifier

    parser =
      try exParentheses
        <|> try exPrimitive
        <|> try exLiteral
        <|> try exOperation
        <|> try exType
        <|> exIdentifier

psApplication :: String -> Parser Expression
psApplication terminator =
  psLexeme (try parser) <|> psClosed <* psSymbol terminator where
    parser =
      ExApplication <$> getSourcePos
        <*> psClosed
        <*> someTill psClosed (psSymbol terminator)

psFunctionType :: String -> Parser Expression
psFunctionType terminator =
  psLexeme (try parser) <|> psApplication terminator where
    parser =
      ExFunctionType <$> getSourcePos
        <*> psFunctionTypeBinding
        <*> psExpression terminator

psFunction :: String -> Parser Expression
psFunction terminator =
  psLexeme (try parser) <|> psFunctionType terminator where
    parser =
      ExFunction <$> getSourcePos
        <*> psFunctionBinding
        <*> psExpression terminator

psSelf :: String -> Parser Expression
psSelf terminator =
  psLexeme (try parser) <|> psFunction terminator where
    parser =
      ExSelf <$> getSourcePos
        <*> psSelfBinding
        <*> psExpression terminator

psData :: String -> Parser Expression
psData terminator =
  psLexeme (try parser) <|> psSelf terminator where
    parser =
      ExData <$> getSourcePos
        <*> (psSymbol "data " *> psExpression terminator)

psCase :: String -> Parser Expression
psCase terminator =
  psLexeme (try parser) <|> psData terminator where
    parser =
      ExCase <$> getSourcePos
        <*> (psSymbol "case " *> psExpression terminator)

psExpression :: String -> Parser Expression
psExpression =
  psCase

psItem :: Parser Item
psItem =
  psLexeme $ Item <$> getSourcePos
    <*> (psSymbol "defn " *> psIdentifier)
    <*> many psBinding
    <*> (psSymbol ":" *> psExpression "{")
    <*> psExpression "}"

psItems :: Parser Items
psItems =
  psLexeme $ Items <$> getSourcePos <*> many psItem <* eof

parse :: String -> String -> Either Error Items
parse input source =
  case Megaparsec.parse psItems input source of
    Left errorBundle -> Left (fromErrorBundle errorBundle)
    Right program -> Right program
