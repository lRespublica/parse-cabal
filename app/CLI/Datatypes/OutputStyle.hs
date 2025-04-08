module CLI.Datatypes.OutputStyle (
    OutputStyle (..),
    stringToOutputStyle,
    outputStyles,
    outputStylesStrings,
    outputStyleParser
 ) where

import Data.List (find)
import Data.List.Extra (enumerate)
import Data.Maybe (listToMaybe, fromMaybe)

import Options.Applicative

data OutputStyle = Plain | JSON | TOML deriving (Enum, Bounded)

instance Show OutputStyle where
    show Plain = "plain"
    show JSON  = "json"
    show TOML  = "toml"

stringToOutputStyle :: String -> Maybe OutputStyle
stringToOutputStyle input = find (\x -> show x == input) outputStyles

instance Read OutputStyle where
    readsPrec _ input = fromMaybe [] result
        where
        result :: Maybe [(OutputStyle, String)]
        result = do
            (readedToken, rest) <- listToMaybe . lex $ input
            parsedStyle <- stringToOutputStyle readedToken
            return [(parsedStyle, rest)]

outputStyles :: [OutputStyle]
outputStyles = enumerate::[OutputStyle]

outputStylesStrings :: [String]
outputStylesStrings = show <$> outputStyles

outputStyleParser :: Parser OutputStyle
outputStyleParser = option auto
   ( long "to"
  <> help "Output style"
  <> completeWith outputStylesStrings
  <> value Plain
  <> metavar (unwords outputStylesStrings)
  <> showDefault
  )
