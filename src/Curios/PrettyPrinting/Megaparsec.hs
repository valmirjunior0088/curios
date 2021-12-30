{-# LANGUAGE NamedFieldPuns #-}

module Curios.PrettyPrinting.Megaparsec
  ( showFile
  , showSource
  )
  where

import Curios.PrettyPrinting.Framed (framed)
import Text.Megaparsec (SourcePos (..), unPos)

showFile :: SourcePos -> String
showFile (SourcePos { sourceName }) =
  "In file \"" ++ sourceName ++ "\"..." ++ "\n"

showSource :: SourcePos -> String -> String
showSource (SourcePos { sourceLine, sourceColumn }) source =
  framed 5 40 (unPos sourceLine - 1) (unPos sourceColumn - 1) source
