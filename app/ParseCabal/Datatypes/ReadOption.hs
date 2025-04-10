module ParseCabal.Datatypes.ReadOption (
    ReadOption (..),
    readOptionsParser,
    getDescription
 ) where

import Options.Applicative

import ParseCabal.Utils (
    PackageDescription (..),
    readFinalPackageDescription,
    getPackageDesc
 )

data ReadOption = CabalFile !FilePath
                | CurrentDir deriving (Show, Eq)

fromFile :: Parser ReadOption
fromFile = CabalFile <$> strOption (
    long "file"
 <> short 'f'
 <> metavar "FILENAME"
 <> action "file"
 <> help "Read data from specified cabal file"
 )

searchFile :: Parser ReadOption
searchFile = flag CurrentDir CurrentDir (
    long "from-current-dir"
 <> help "(default)"
 )

readOptionsParser :: Parser ReadOption
readOptionsParser = fromFile <|> searchFile

getDescription :: ReadOption -> IO PackageDescription
getDescription (CabalFile file) = readFinalPackageDescription [] file
getDescription CurrentDir = getPackageDesc
