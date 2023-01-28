module Intermediate.Debug
  ( debug
  )
  where

import Intermediate.Syntax
  ( Atom (..)
  , Expression (..)
  , Sequence (..)
  , Closure (..)
  , Block (..)
  , Program (..)
  )

import Data.List (intercalate)
import Data.Int (Int32)

debugBranch :: (Int32, String, [Atom]) -> String
debugBranch (label, block, atoms) = "|" ++ show label ++ "| block.Call " ++ block ++ " [" ++ intercalate ", " (map show atoms) ++ "]"

debugCondition :: String -> (String, [Atom]) -> String
debugCondition condition (block, atoms) = "|" ++ condition ++ "| block.Call " ++ block ++ " [" ++ intercalate ", " (map show atoms) ++ "]"

debugExpression :: Expression -> String
debugExpression = \case
  Int32Alloc value -> "int32.Alloc " ++ show value
  Int32Match atom branches -> "int32.Match [" ++ show atom ++ "]:\n" ++ intercalate "\n" (map debugBranch branches)
  Int32If atom truthy falsy -> "int32.If [" ++ show atom ++ "]:\n" ++ debugCondition "true" truthy ++ "\n" ++ debugCondition "false" falsy
  Int32BinOp op one other -> "int32." ++ show op ++ " [" ++ show one ++ "] [" ++ show other ++ "]"
  Int32BoolOp op one other -> "int32." ++ show op ++ " [" ++ show one ++ "] [" ++ show other ++ "]"
  Int32CompOp op one other -> "int32." ++ show op ++ " [" ++ show one ++ "] [" ++ show other ++ "]"
  Flt32Alloc value -> "flt32.Alloc " ++ show value
  Flt32BinOp op one other -> "flt32." ++ show op ++ " [" ++ show one ++ "] [" ++ show other ++ "]"
  Flt32CompOp op one other -> "flt32." ++ show op++ " [" ++ show one ++ "] [" ++ show other ++ "]"
  Pure atom -> "Pure [" ++ show atom ++ "]"
  BlockCall block atoms -> "block.Call " ++ block ++ " [" ++ intercalate ", " (map show atoms) ++ "]"
  ClosureAlloc closure atoms -> "closure.Alloc " ++ closure ++ " {" ++ intercalate ", " (map show atoms) ++ "}"
  ClosureEnter atom atoms -> "closure.Enter [" ++ show atom ++ "] [" ++ intercalate ", " (map show atoms) ++ "]"
  StructAlloc atoms -> "struct.Alloc [" ++ intercalate ", " (map show atoms) ++ "]"
  StructSelect atom index -> "struct.Select [" ++ show atom ++ "] " ++ show index

debugSequence :: Sequence -> [String]
debugSequence = \case
  Bind name body rest -> (name ++ " <- " ++ debugExpression body ++ ";") : debugSequence rest
  Tail body -> [debugExpression body]

debugBody :: Sequence -> String
debugBody body = intercalate "\n" $ map ("  " ++) $ debugSequence body

debugClosure :: (String, Closure) -> String
debugClosure (name, Closure { environment, parameters, body }) = do
  let
    environmentString = "{" ++ intercalate ", " environment ++ "}"
    parametersString = "[" ++ intercalate ", " parameters ++ "]"
    bodyString = "do\n" ++ debugBody body ++ "\nend"

  "closure " ++ name ++ " "
    ++ environmentString ++ " "
    ++ parametersString ++ " "
    ++ bodyString

debugBlock :: (String, Block) -> String
debugBlock (name, Block { parameters, body }) = do
  let
    parametersString = "[" ++ intercalate ", " parameters ++ "]"
    bodyString = "do\n" ++ debugBody body ++ "\nend"

  "block " ++ name ++ " "
    ++ parametersString ++ " "
    ++ bodyString

debug :: Program -> String
debug Program { closures, blocks } =
  intercalate "\n\n" (map debugClosure closures ++ map debugBlock blocks)
