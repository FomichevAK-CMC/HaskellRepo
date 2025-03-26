module Main (main) where

import System.Environment (getArgs)
import Data.List (intercalate)
import Text.Read (readMaybe)
import Data.Char (isSpace)

-- | JSON value representation
data JsonValue
  = JsonObject [(String, JsonValue)]  -- Key-value pairs
  | JsonArray [JsonValue]             -- List of values
  | JsonString String                 -- String value
  | JsonNumber Double                 -- Numeric value
  | JsonBool Bool                     -- Boolean value
  | JsonNull                          -- Null value
  deriving (Show)

-- | Type alias for a line with its indentation level
type Line = (Int, String)

-- | YAML node in the parse tree
data YamlNode = YamlNode
  { indentLevel :: Int         -- Indentation level
  , content     :: String      -- Line content after indentation
  , children    :: [YamlNode]  -- Nested nodes
  }

-- | Split YAML content into lines with indentation levels
splitLines :: String -> [Line]
splitLines input =
  [ (length (takeWhile (== ' ') line), dropWhile (== ' ') line)
  | line <- lines input
  , let trimmed = dropWhile isSpace line
  , not (null trimmed) && head trimmed /= '#'  -- Ignore empty lines and comments
  ]

-- | Build a tree of YAML nodes based on indentation
buildTree :: Int -> [Line] -> ([YamlNode], [Line])
buildTree indent lines =
  let (nodes, remaining) = go [] lines
  in (reverse nodes, remaining)
  where
    go acc [] = (acc, [])
    go acc ((ind, cont):rest)
      | ind < indent = (acc, (ind, cont):rest)  -- Return to parent level
      | ind == indent =
          let (childNodes, rest') = buildTree (indent + 2) rest  -- Assume 2-space indent
          in go (YamlNode ind cont childNodes : acc) rest'
      | otherwise = error "Unexpected indentation"

-- | Parse a block of nodes into a JsonValue
parseBlock :: [YamlNode] -> JsonValue
parseBlock nodes
  -- Single node with no children and no colon is a scalar
  | length nodes == 1 && null (children (head nodes)) && not (':' `elem` content (head nodes)) =
      parseScalar (content (head nodes))
  -- All nodes start with "- " indicates a sequence
  | all (\n -> take 2 (content n) == "- ") nodes =
      JsonArray [parseSequenceItem node | node <- nodes]
  -- Otherwise, treat as a mapping
  | otherwise =
      JsonObject [parseMappingItem node | node <- nodes]

-- | Parse a sequence item
parseSequenceItem :: YamlNode -> JsonValue
parseSequenceItem node
  | take 2 (content node) == "- " =
      let itemContent = drop 2 (content node)
      in if null itemContent
         then if null (children node)
              then JsonNull  -- Empty sequence item
              else parseBlock (children node)  -- Nested structure
         else parseScalar itemContent  -- Inline scalar
  | otherwise = error "Invalid sequence item"

-- | Parse a mapping item into a key-value pair
parseMappingItem :: YamlNode -> (String, JsonValue)
parseMappingItem node =
  case break (== ':') (content node) of
    (key, ':':rest) ->
      let valueContent = dropWhile isSpace rest
      in (key, if null valueContent
               then if null (children node)
                    then JsonNull  -- No value specified
                    else parseBlock (children node)  -- Nested structure
               else parseScalar valueContent)  -- Inline scalar
    _ -> error "Invalid mapping item"

-- | Parse a scalar value into appropriate JsonValue
parseScalar :: String -> JsonValue
parseScalar s
  | s == "null" || s == "~" || s == "" = JsonNull
  | s == "true" = JsonBool True
  | s == "false" = JsonBool False
  | otherwise =
      case readMaybe s :: Maybe Double of
        Just n -> JsonNumber n
        Nothing -> JsonString s

-- | Convert JsonValue to a JSON string
jsonToString :: JsonValue -> String
jsonToString (JsonObject pairs) =
  "{" ++ intercalate "," [ "\"" ++ escape key ++ "\":" ++ jsonToString val | (key, val) <- pairs ] ++ "}"
jsonToString (JsonArray vals) =
  "[" ++ intercalate "," [ jsonToString val | val <- vals ] ++ "]"
jsonToString (JsonString s) = "\"" ++ escape s ++ "\""
jsonToString (JsonNumber n) = show n
jsonToString (JsonBool b) = if b then "true" else "false"
jsonToString JsonNull = "null"

-- | Escape special characters in a string
escape :: String -> String
escape = concatMap escapeChar
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c    = [c]

-- | Parse YAML string into a JsonValue
parseYaml :: String -> JsonValue
parseYaml input =
  let allLines = splitLines input
      (nodes, _) = buildTree 0 allLines
  in parseBlock nodes

-- | Main function to run the program
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      let jsonValue = parseYaml content
      putStrLn (jsonToString jsonValue)
    _ -> putStrLn "Usage: yaml2json <filename>"