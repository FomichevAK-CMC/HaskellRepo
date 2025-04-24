{-# LANGUAGE LambdaCase #-}
module MyParser  where
import YAMLLexer

-- main yaml types (not all are supported yet)
data YamlValue
    = YamlScalar String ScalarStyle
    | YamlMap [(YamlValue, YamlValue)]
    | YamlSeq [YamlValue]
    | YamlPlain String
    | YamlStr String
    | YamlAlias String -- Alias Name (Anchor ref)
    | YamlEmpty
    deriving (Show, Eq)

-- preprocessed yaml token types
data PreprocYamlToken
    = PreYamlKV YamlToken YamlToken
    | PreYamlK YamlToken -- key without specified value
    | PreYamlSeqItem YamlToken
    | PreYamlSeq 
    | PreYamlDocStart
    | PreYamlDocEnd
    | PreYamlScalar YamlToken
    | PreYamlEof
    deriving (Show, Eq)

-- preprocesses lexer tokens assigning them basic semantic roles
preprocYamlTokens :: [(Int, YamlToken)] -> [(Int, PreprocYamlToken)]
preprocYamlTokens tokens = _preprocYamlTokens $ stripRedundant tokens
_preprocYamlTokens :: [(Int, YamlToken)] -> [(Int, PreprocYamlToken)]
_preprocYamlTokens [] = []
_preprocYamlTokens tokens =
    let (t, rest2) =
            case tokens of
                ((i, key@(SCALAR _ _)):(_, COLON):rest) -> ((i, PreYamlK key), rest)
                ((i, sc@(SCALAR _ _)):rest) -> ((i, PreYamlScalar sc), rest)
                ((i, DASH):rest) -> ((i, PreYamlSeq), rest)
                ((_, DOC_START):rest) -> ((0, PreYamlDocStart), rest)
                ((_, DOC_END):rest)   -> ((0, PreYamlDocEnd), rest)
                ((_, EOF):rest)       -> ((0, PreYamlEof), rest)
                ((_,other):_) -> error $ "ERROR (_preprocYamlTokens): unsupported token <"
                                  ++ (show other) ++ ">!\n"
    in (t:_preprocYamlTokens rest2)


data YamlDocument = YamlDoc YamlValue deriving (Show, Eq)

-- arranges a series of preprocessed yaml tokens into a structure based on indentation
extractStructure :: [(Int, PreprocYamlToken)] -> [YamlDocument] -- TODO: Add document support
extractStructure [] = error $ "ERROR (extractStruncture): incomplete structure!"
extractStructure tokens@((_, PreYamlDocStart):rest) = _extractStructure tokens
extractStructure tokens = _extractStructure ((0, PreYamlDocStart):tokens)

_extractStructure :: [(Int, PreprocYamlToken)] -> [YamlDocument]
_extractStructure [] = error $ "ERROR (extractStruncture): incomplete structure!"
_extractStructure ((i, PreYamlDocStart):rest) = let (val, rest2) = _extractStructure_1 rest i in
                                                    (YamlDoc val):(_extractStructure rest2)
_extractStructure ((_, PreYamlDocEnd):(_, PreYamlEof):rest) = []
_extractStructure ((_, PreYamlDocEnd):rest) = (YamlDoc YamlEmpty):(_extractStructure rest)
_extractStructure ((_, PreYamlEof):_) = []
_extractStructure tokens@((i, _):_) = error $ "ERROR (_extractStructure): inconsistent yaml structure!" 


_extractStructure_1 :: [(Int, PreprocYamlToken)] -> Int -> (YamlValue, [(Int, PreprocYamlToken)])
_extractStructure_1 [] _ = error $ "ERROR (_extractStruncture_1): incomplete structure!"
_extractStructure_1 tokens@((i, token):rest) ind
    | i < ind = error $ "ERROR (_extractStructure_seq): inconsistent indentation for token "
                        ++ (show token)++ "! Got " ++ (show i) ++ ", expected " ++ (show ind) ++ "."
    | otherwise =
        case token of
            (PreYamlScalar (SCALAR str style)) -> _extractStructure_str tokens i
            PreYamlK _      -> _extractStructure_map tokens i
            PreYamlSeq      -> _extractStructure_seq tokens i
            PreYamlDocEnd   -> (YamlEmpty, rest)
            PreYamlDocStart -> (YamlEmpty, tokens)
            PreYamlEof      -> (YamlEmpty, tokens)
            _other -> error $ "ERROR (_extractStructure_1): unsupported token <"
                              ++ (show token) ++ ">!\n"
_extractStructure_str :: [(Int, PreprocYamlToken)] -> Int -> (YamlValue, [(Int, PreprocYamlToken)])
_extractStructure_str tokens@((i, (PreYamlScalar (SCALAR str style))):next:rest) ind =
    case next of
        (_, PreYamlDocEnd) -> ((YamlScalar str style), rest)
        _                  -> ((YamlScalar str style), next:rest)

_extractStructure_seq :: [(Int, PreprocYamlToken)] -> Int -> (YamlValue, [(Int, PreprocYamlToken)])
_extractStructure_seq [] _ = error $ "ERROR (_extractStruncture_seq): incomplete structure!"
_extractStructure_seq tokens@((i, token):rest) ind
    | i > ind = error $ "ERROR (_extractStructure_seq): inconsistent indentation for token "
                        ++ (show token)++ "! Got " ++ (show i) ++ ", expected " ++ (show ind) ++ "."
    | i < ind = ((YamlSeq []), tokens)
    | otherwise =
        case token of
            PreYamlSeq ->
                let (s, rest2) = _extractStructure_1 rest i in
                    let (YamlSeq ss, rest3) = _extractStructure_seq rest2 ind in -- ignore wanring
                        (YamlSeq (s:ss), rest3)
            PreYamlK _      -> ((YamlSeq []), tokens)
            PreYamlEof      -> ((YamlSeq []), tokens)
            PreYamlDocEnd   -> ((YamlSeq []), rest)
            PreYamlDocStart -> ((YamlSeq []), tokens)
            _ -> error $ "ERROR (_extractStructure_seq): inconsistent sequence structure (got "
                         ++ (show token) ++ " token with <" ++ (show i) ++ "> indent)!\n"

_extractStructure_map :: [(Int, PreprocYamlToken)] ->  Int -> (YamlValue, [(Int, PreprocYamlToken)])
_extractStructure_map [] _ = error $ "ERROR (_extractStruncture_map): incomplete structure!"
_extractStructure_map tokens@((i, token):rest) ind
    | i > ind = error $ "ERROR (_extractStructure_map): inconsistent indentation for token "
                        ++ (show token)++ "! Got " ++ (show i) ++ ", expected " ++ (show ind) ++ "."
    | i < ind = ((YamlMap []), tokens)
    | otherwise =
        case token of
            (PreYamlK (SCALAR str style)) ->
                let (s, rest2) = _extractStructure_1 rest i in
                    let (YamlMap ss, rest3) = _extractStructure_map rest2 ind in -- ignore warning
                        (YamlMap ((YamlScalar str style, s):ss), rest3)
            PreYamlEof      -> ((YamlMap []), tokens)
            PreYamlDocEnd   -> ((YamlMap []), rest)
            PreYamlDocStart -> ((YamlMap []), tokens)
            _ -> error $ "ERROR (_extractStructure_map): inconsistent mapping structure (got "
                          ++ (show token) ++ " token with <" ++ (show i) ++ "> indent)!\n"

digits :: String
digits = "0123456789"
digitsnz :: String
digitsnz = "123456789"

-- recursive descend to check if a string is a valid json number
isJsonNumber :: String -> Bool
isJsonNumber "" = False
isJsonNumber (s:xs) | (s == '-') = _isJsonNumber_int_1 xs
                    | otherwise = _isJsonNumber_int_1 (s:xs)
_isJsonNumber_int_1 :: String -> Bool
_isJsonNumber_int_1 "" = False
_isJsonNumber_int_1 (s:xs) | (elem s digitsnz) = _isJsonNumber_int_2 xs
                           | (s == '0') = _isJsonNumber_frc_1 xs
                           | otherwise = False
_isJsonNumber_int_2 :: String -> Bool
_isJsonNumber_int_2 "" = True
_isJsonNumber_int_2 (s:xs) | (elem s digits) = _isJsonNumber_int_2 xs
                           | otherwise = _isJsonNumber_frc_1 (s:xs)
_isJsonNumber_frc_1 :: String -> Bool
_isJsonNumber_frc_1 "" = True
_isJsonNumber_frc_1 (s:xs) | (s == '.') = _isJsonNumber_frc_2 xs
                           | otherwise = _isJsonNumber_exp_1 (s:xs)
_isJsonNumber_frc_2 :: String -> Bool
_isJsonNumber_frc_2 "" = False
_isJsonNumber_frc_2 (s:xs) | (elem s digits) = _isJsonNumber_frc_3 xs
                           | otherwise = False
_isJsonNumber_frc_3 :: String -> Bool
_isJsonNumber_frc_3 "" = True
_isJsonNumber_frc_3 (s:xs) | (elem s digits) = _isJsonNumber_frc_3 xs
                           | otherwise = _isJsonNumber_exp_1 (s:xs)
_isJsonNumber_exp_1 :: String -> Bool
_isJsonNumber_exp_1 "" = True
_isJsonNumber_exp_1 (s:xs) | (elem s "eE") = _isJsonNumber_exp_2 xs
                           | otherwise = False
_isJsonNumber_exp_2 :: String -> Bool
_isJsonNumber_exp_2 "" = False
_isJsonNumber_exp_2 (s:xs) | (elem s "-+") = _isJsonNumber_exp_3 xs
                           | otherwise = _isJsonNumber_exp_3 (s:xs)
_isJsonNumber_exp_3 :: String -> Bool
_isJsonNumber_exp_3 "" = False
_isJsonNumber_exp_3 (s:xs) | (elem s digits) = _isJsonNumber_exp_4 xs
                           | otherwise = False
_isJsonNumber_exp_4 :: String -> Bool
_isJsonNumber_exp_4 "" = True
_isJsonNumber_exp_4 (s:xs) | (elem s digits) = _isJsonNumber_exp_4 xs
                           | otherwise = False

isYamlTrue :: String -> Bool
isYamlTrue str = elem str ["y", "Y", "yes", "Yes", "YES", "true", "True", "TRUE", "on", "On", "ON"]
isYamlFalse :: String -> Bool
isYamlFalse str = elem str ["n","N","no", "No","NO", "false", "False", "FALSE", "off", "Off", "OFF"]
isYamlNull :: String -> Bool
isYamlNull str = elem str ["", "~", "null", "Null", "NULL"]

-- iterates over structure and assigns proper types to scalars
inferScalarTypes :: YamlValue -> YamlValue
inferScalarTypes YamlEmpty = YamlEmpty
inferScalarTypes (YamlScalar str PlainStyle)
    | isYamlTrue str = YamlPlain "true"
    | isYamlFalse str = YamlPlain "false"
    | isYamlNull str = YamlPlain "null"
    | isJsonNumber str = YamlPlain str
    | otherwise = YamlStr str
inferScalarTypes (YamlScalar str _) = YamlStr str
inferScalarTypes (YamlSeq items) = YamlSeq (map inferScalarTypes items)
inferScalarTypes (YamlMap kvs) =
    YamlMap (map (\(a, b) -> (inferScalarTypes a, inferScalarTypes b)) kvs)
inferScalarTypes t = error $ "ERROR (inferScalarTypes): improper structure that contains: "
                             ++ (show t) ++ "!\n"

-- main function that converts yaml string into json string
yamlToJson :: String -> String
yamlToJson yamlstr = _yamlToJson_docs (extractStructure $ preprocYamlTokens $ lexer yamlstr)

_yamlToJson_docs :: [YamlDocument] -> String
_yamlToJson_docs docs = _yamlToJson (YamlSeq (map (\(YamlDoc v) -> inferScalarTypes v) docs)) 0 0
_yamlToJson :: YamlValue -> Int -> Int -> String
_yamlToJson YamlEmpty _ ind_val = (indentStr ind_val "null")
_yamlToJson (YamlMap []) _ _ = "{}"
_yamlToJson (YamlMap kvs) ind_start ind_val = (indentStr ind_start "{\n")
                                              ++ (_yamlToJson_map kvs (ind_val + 2))
                                              ++ "\n" ++ (indentStr ind_val "}")
_yamlToJson (YamlSeq []) _ _ = "[]"
_yamlToJson (YamlSeq items) ind_start ind_val = (indentStr ind_start "[\n")
                                                ++ (_yamlToJson_seq items (ind_val + 2))
                                                ++ "\n" ++ (indentStr ind_val "]")
_yamlToJson (YamlPlain str) ind_start _ = (indentStr ind_start str)
_yamlToJson (YamlStr str) ind_start _ = (indentStr ind_start (show str))
_yamlToJson t _ _ = error $ "ERROR (_yamlToJson): unsupported token " ++ (show t) ++ "!\n"

_yamlToJson_map :: [(YamlValue, YamlValue)] -> Int -> String
_yamlToJson_map [] _ = error $ "ERROR (_yamlToJson_map): empty structure passed!"
_yamlToJson_map ((key, val):[]) ind = (_yamlToJson key ind ind)
                                      ++ ": " ++ (_yamlToJson val 0 ind)
_yamlToJson_map ((key, val):rest) ind = (_yamlToJson key ind ind)
                                        ++ ": " ++ (_yamlToJson val 0 ind)
                                        ++ ",\n" ++ (_yamlToJson_map rest ind)

_yamlToJson_seq :: [YamlValue] -> Int -> String
_yamlToJson_seq [] _ = error $ "ERROR (_yamlToJson_seq): empty structure passed!"
_yamlToJson_seq (item:[]) ind = (_yamlToJson item ind ind)
_yamlToJson_seq (item:rest) ind = (_yamlToJson item ind ind)
                                  ++ ",\n" ++ (_yamlToJson_seq rest ind)

-- returns string padded with given number of spaces
indentStr :: Int -> String -> String
indentStr ind str = (replicate ind ' ') ++ str

stripRedundant :: [(Int, YamlToken)] -> [(Int, YamlToken)]
stripRedundant tokens = filter (\case 
                                (_, COMMENT _) -> False;
                                (_, DIRECTIVE _) -> False;
                                (_, SCALAR "" _) -> False; _ -> True) tokens





