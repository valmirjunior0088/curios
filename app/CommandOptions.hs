module CommandOptions
  (CommandOptions (..)
  ,coParser
  ,coMain
  )
  where

import Data.Semigroup
  ((<>)
  )

import Options.Applicative
  (Parser
  ,argument
  ,str
  ,metavar
  ,optional
  ,subparser
  ,command
  ,info
  ,fullDesc
  ,progDesc
  ,execParser
  ,helper
  ,header
  )

data CommandOptions =
  CoPrint String |
  CoCheck String (Maybe String)
  deriving (Show)

coParser :: Parser CommandOptions
coParser =
  subparser (coPrintCommand <> coCheckCommand) where
    coPrintParser =
      CoPrint <$> argument str (metavar "EXPRESSION")

    coPrintCommand =
      command "print" (info (helper <*> coPrintParser)
        (progDesc "Prints the representation of an expression"))
    
    coCheckParser =
      CoCheck <$> argument str (metavar "PATH") <*> optional (argument str (metavar "NAME"))

    coCheckCommand =
      command "check" (info (helper <*> coCheckParser)
        (progDesc "Typechecks a source file"))

coMain :: (CommandOptions -> IO ()) -> IO ()
coMain coRun =
  coRun =<< execParser
    (info (helper <*> coParser)
      (fullDesc <> header "Curios - a dependently typed functional programming language")
    )
