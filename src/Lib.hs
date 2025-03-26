module Lib
    (
    ) where

-- yaml scalar values
data YamlScalar =
      YString String
    | YInt Int
    | YFloat Float
    | YBool Bool
    | YNull

-- yaml structure modifiers
data YamlStruct = 
    YTag String
  | YCustomTag String
  | YAnchor String
  | YAlias String
  | YMerge YAlias

-- yaml collection types
data YamlCollection = 
    YList [YamlBody]
  | YMap [(YBScalar, YamlBody)]

-- main yaml types
data YamlBody = YBScalar YamlScalar | YBCollection YamlCollection | YBStruct YamlStruct


-- json scalar values
data JsonScalar =
      JString String
    | JInt Int
    | JFloat Float
    | JBool Bool
    | JNull

-- json collection types
data JsonCollection =
      JObject [(JString, JsonBody)]
    | JArray [JsonBody]

-- main json types
data JsonBody = JBScalar JsonScalar | JBCollection JsonCollection


parseYAML :: IO String -> [(Int, String)]
-- parses YAML file into individual unclassified parts with indentations
-- indentation values are unique per line
-- does not check for markup errors

tokenizeYAML :: [(Int, String)] -> YamlBody
-- recieves parsed YAML code and returns YamlBody
-- checks for markup errors

dealiasYAML :: YamlBody -> YamlBody
-- resolves anchor->alias pairs in YamlBody resulting in YamlBody without any anchors or aliases

dealiasYAML_rec :: YamlBody -> [(String, YamlBody)] -> YamlBody
-- helper function for dealiasYAML that additionally accepts a map of anchored structures 

yamlToJson :: YamlBody -> JsonBody
-- accepts YamlBody, dealiases it and converts to JsonBody

jsonToStr :: JsonBody -> String
-- converts JsonBody to a String

