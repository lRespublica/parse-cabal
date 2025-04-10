module Main where

import Options.Applicative

import ParseCabal.Datatypes.OutputStyle
import ParseCabal.Datatypes.ReadOption
import ParseCabal.Datatypes.LookupValue

import ParseCabal.Writer

import Control.Monad (join)
import Data.List.Extra (nubOrd)
import Data.List (intercalate)

data CLIOptions = CLIOptions
  { parsedOutputStyle  :: !OutputStyle
  , parsedReadOption   :: !ReadOption
  , parsedLookupValues :: ![[LookupValue]] } deriving (Show)

cliOptionsParser :: Parser CLIOptions
cliOptionsParser = CLIOptions <$>
                   outputStyleParser <*>
                   readOptionsParser <*>
                   some allLookupValueParser

main :: IO ()
main = do
    parsedOpts <- execParser opts
    let searchValues = nubOrd . join . parsedLookupValues $ parsedOpts
        readFrom = parsedReadOption parsedOpts
        outputStyle = parsedOutputStyle parsedOpts

    desc <- getDescription readFrom

    let outputValues = fmap (\x -> toOutputValue outputStyle x desc) searchValues
        outputStr = beginOutput outputStyle ++
                    intercalate (separatorFor outputStyle) outputValues ++
                    endOutput outputStyle

    putStrLn outputStr

  where
    opts = info (cliOptionsParser <**> helper)
      ( fullDesc
     <> header "Parse a cabal file")
