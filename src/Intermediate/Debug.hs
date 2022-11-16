module Intermediate.Debug
  ( debug
  )
  where

import Intermediate.Syntax (Expression (..), Sequence (..), Closure (..), Block (..), Program (..))
import Data.List (intercalate)
import Data.Int (Int32)

debugBranch :: (Int32, Expression) -> String
debugBranch (label, expression) = "|" ++ show label ++ "| " ++ debugExpression expression

debugExpression :: Expression -> String
debugExpression = \case
  Int32Alloc value -> "int32.alloc " ++ show value
  Int32Add one other -> "int32.add [" ++ show one ++ ", " ++ show other ++ "]"
  Flt32Alloc value -> "flt32.alloc " ++ show value
  Flt32Add one other -> "flt32.add [" ++ show one ++ ", " ++ show other ++ "]"
  Pure atom -> "pure [" ++ show atom ++ "]"
  BlockCall block atoms -> "block.call " ++ block ++ " [" ++ intercalate ", " (map show atoms) ++ "]"
  Int32Match atom branches -> "int32.match [" ++ show atom ++ "]: " ++ unwords (map debugBranch branches)
  ClosureAlloc closure atoms -> "closure.alloc " ++ closure ++ " {" ++ intercalate ", " (map show atoms) ++ "}"
  ClosureEnter atom atoms -> "closure.enter [" ++ show atom ++ "] [" ++ intercalate ", " (map show atoms) ++ "]"
  StructAlloc atoms -> "struct.alloc [" ++ intercalate ", " (map show atoms) ++ "]"
  StructSelect atom index -> "struct.select [" ++ show atom ++ "] " ++ show index

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
