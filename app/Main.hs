import Prelude hiding (error)

import Curios.Error (Kind (..), Error (..), erParsing)
import Curios.Context (Context (..), pgCheck, cnLookupDeclaration, cnLookupDefinition)
import Curios.Formatting (putFramedLn)
import Curios.Source.Types (Program (..))
import qualified Curios.Source.Parser as Parser (program)
import Curios.Core.Term (Origin (..), Name (..), trShow)
import qualified Text.Megaparsec as Megaparsec (parse)
import Text.Megaparsec (ParseErrorBundle (..), SourcePos (..), unPos, parseErrorTextPretty, errorOffset)
import Text.Megaparsec.Stream (reachOffset)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Bifunctor (first)
import System.Environment (getArgs)
import System.IO (IOMode (..), openFile, hGetContents)

parse :: String -> String -> Either Error Program
parse file source =
  first transform (Megaparsec.parse Parser.program file source) where
    transform parseErrorBundle =
      erParsing (OrSource sourcePos) error where
        (error :| _) = bundleErrors parseErrorBundle
        (sourcePos, _, _) = reachOffset (errorOffset error) (bundlePosState parseErrorBundle)

check :: String -> String -> Either Error Context
check file source =
  do
    program <- parse file source

    pgCheck program

handleError :: String -> String -> Error -> IO ()
handleError file source error =
  do
    putStrLn ("Check failed.")
    putStrLn ("")
    putStrLn ("In file " ++ file ++ "...")

    case erOrigin error of
      OrMachine ->
        do
          putStrLn ("In a machine-generated term...")
      OrSource sourcePos ->
        do
          let line = unPos (sourceLine sourcePos)
          let column = unPos (sourceColumn sourcePos)

          putStrLn ("In line " ++ show line ++ ", column " ++ show column ++ "...")
          putStrLn ("")
          putFramedLn 3 60 (line - 1) (column - 1) source
          putStrLn ("")

    case erKind error of
      KnParsing parseError ->
        do
          putStrLn ("Parse error.")
          putStrLn (parseErrorTextPretty parseError)
      KnUndeclaredName name ->
        do
          putStrLn ("The name \"" ++ show name ++ "\" is undeclared.")
      KnRepeatedlyDeclaredName name ->
        do
          putStrLn ("The name \"" ++ show name ++ "\" is repeatedly declared.")
      KnRepeatedlyDefinedName name ->
        do
          putStrLn ("The name \"" ++ show name ++ "\" is repeatedly defined.")
      KnMismatchedFunctionType obtainedType ->
        do
          putStrLn ("Type mismatch.")
          putStrLn ("- Expected: <function type>")
          putStrLn ("- Obtained: " ++ trShow obtainedType)
      KnMismatchedType expectedType obtainedType ->
        do
          putStrLn ("Type mismatch.")
          putStrLn ("- Expected: " ++ trShow expectedType)
          putStrLn ("- Obtained: " ++ trShow obtainedType)
      KnFunctionNotInferable ->
        do
          putStrLn ("The types of functions are not inferable without annotations.")

handleContext :: String -> Context -> IO ()
handleContext name context =
  do
    putStrLn ("Check succeeded!")
    putStrLn ("")
    putStrLn ("Declaration:")
    putStrLn (show (fmap trShow (cnLookupDeclaration (Name name) context)))
    putStrLn ("")
    putStrLn ("Definition:")
    putStrLn (show (fmap trShow (cnLookupDefinition (Name name) context)))

main :: IO ()
main =
  do
    arguments <- getArgs

    putStrLn "Curios - a web of types"

    case arguments of
      [file, name] ->
        do
          source <- openFile file ReadMode >>= hGetContents

          case check file source of
            Left error -> handleError file source error
            Right context -> handleContext name context

      [file] ->
        do
          source <- openFile file ReadMode >>= hGetContents

          case check file source of
            Left error -> handleError file source error
            Right _ -> putStrLn "Check succeeded!"
      _ ->
        do
          putStrLn "USAGE: <command> file [name]"
