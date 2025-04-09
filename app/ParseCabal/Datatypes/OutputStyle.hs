module ParseCabal.Datatypes.OutputStyle (
    OutputStyle (..),
    outputStyleParser
 ) where

import Data.List.Extra (enumerate)
import Data.Char (toLower)

import Options.Applicative

data OutputStyle = Plain | JSON | TOML deriving (Enum, Bounded, Show, Read)

outputStyles :: [OutputStyle]
outputStyles = enumerate::[OutputStyle]

outputStylesStrings :: [String]
outputStylesStrings = map ((toLower <$>) . show) outputStyles

associatedOutputStyles :: [(String, OutputStyle)]
associatedOutputStyles = zip outputStylesStrings outputStyles

stringToOutputStyle :: String -> Maybe OutputStyle
stringToOutputStyle input = lookup input associatedOutputStyles

outputStyleParser :: Parser OutputStyle
outputStyleParser = option (maybeReader stringToOutputStyle)
   ( long "to"
  <> help "Output style"
  <> completeWith outputStylesStrings
  <> value Plain
  <> metavar (unwords outputStylesStrings)
  <> showDefault
  )
