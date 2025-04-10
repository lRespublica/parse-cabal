module ParseCabal.Writer (
    beginOutput,
    toOutputValue,
    endOutput,
    separatorFor
 ) where

import ParseCabal.Datatypes.LookupValue
import ParseCabal.Datatypes.OutputStyle
import ParseCabal.Utils (PackageDescription)

import Data.List.Extra

showLower :: Show a => a -> String
showLower = lower . show

beginOutput :: OutputStyle -> String
beginOutput JSON  = "{\n"
beginOutput _  = ""

toArray :: OutputStyle -> String -> String
toArray Plain = id
toArray _ = show . fmap escapeJSON . lines

toOutputFormat :: LookupValue -> OutputStyle -> String -> String
toOutputFormat value style str | isPlural value = toArray style str
                               | otherwise = show . escapeJSON $ str

toOutputValue :: OutputStyle -> LookupValue -> PackageDescription -> String
toOutputValue Plain lookupValue desc = searchForValue lookupValue desc
toOutputValue TOML lookupValue desc = showLower lookupValue ++ " = " ++ output
    where foundValue = searchForValue lookupValue desc
          output = toOutputFormat lookupValue TOML foundValue
toOutputValue JSON lookupValue desc = key ++ ": " ++ output
    where foundValue = searchForValue lookupValue desc
          output = toOutputFormat lookupValue JSON foundValue
          key = show $ showLower lookupValue

endOutput :: OutputStyle -> String
endOutput JSON = "\n}"
endOutput _ = ""

separatorFor :: OutputStyle -> String
separatorFor Plain = "\n\n"
separatorFor TOML  = "\n"
separatorFor JSON  = ",\n"
