module ParseCabal.Datatypes.LookupValue (
    LookupValue (..),
    allLookupValueParser,
    searchForValue
 ) where

import Data.List.Extra (enumerate)
import Data.Char (toLower)

import Options.Applicative

import ParseCabal.Utils

data LookupValue = Name | Version | PkgId | License
                 | Description | Synopsis | Homepage
                 | BuildDeps | Executables  deriving (Enum, Bounded, Show, Read)

lookupValues :: [LookupValue]
lookupValues = enumerate::[LookupValue]

lookupValuesStrings :: [String]
lookupValuesStrings = map ((toLower <$>) . show) lookupValues

associatedLookupValues :: [(String, LookupValue)]
associatedLookupValues = zip lookupValuesStrings lookupValues

stringToLookupValue :: String -> Maybe LookupValue
stringToLookupValue input = lookup input associatedLookupValues

lookupValueParser :: ReadM [LookupValue]
lookupValueParser = eitherReader $ \s ->
  if map toLower s == "all"
    then Right lookupValues
    else case stringToLookupValue s of
      Just lv -> Right [lv]
      Nothing -> Left $ s ++ " is not an option!"

allLookupValueParser :: Parser [LookupValue]
allLookupValueParser = argument lookupValueParser
   ( metavar (unwords $ "all" : lookupValuesStrings)
  <> completeWith ("all" : lookupValuesStrings)
   )

searchForValue :: LookupValue -> PackageDescription -> String
searchForValue Name = getPkgName
searchForValue Version = getPkgVersion
searchForValue PkgId = getPkgId
searchForValue License = getPkgLicenseString
searchForValue Description = getPkgDescriptionText
searchForValue Synopsis = getPkgSynopsis
searchForValue Homepage = getPkgHomepage
searchForValue BuildDeps = getPkgBuildDeps
searchForValue Executables = getPkgExecutables
