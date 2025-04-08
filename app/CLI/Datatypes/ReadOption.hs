module CLI.Datatypes.ReadOption (
    ReadOption (..),
    readOptionsParser
 ) where

import Options.Applicative

data ReadOption = CabalFile !FilePath
                | CurrentDir deriving (Show, Eq)

fromFile :: Parser ReadOption
fromFile = CabalFile <$> strOption (
    long "file"
 <> short 'f'
 <> metavar "FILENAME"
 <> help "Read data from specified cabal file"
 )

searchFile :: Parser ReadOption
searchFile = flag CurrentDir CurrentDir (
    long "from-current-dir"
 <> help "(default)"
 )

readOptionsParser :: Parser ReadOption
readOptionsParser = fromFile <|> searchFile
