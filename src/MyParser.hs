{-# LANGUAGE LambdaCase #-}
module MyParser (yamlToJson) where
import YAMLLexer

data YamlValue
    = YamlScalar String ScalarStyle --(Maybe String) (Maybe String) -- Value, Style, Maybe Anchor, Maybe Tag
    | YamlMap [(YamlValue, YamlValue)] --(Maybe String) (Maybe String) -- Ass K-V pairs, Maybe Anchor, Maybe Tag
    | YamlSeq [YamlValue] --(Maybe String) (Maybe String)             -- List of values, Maybe Anchor, Maybe Tag
    | YamlAlias String -- Alias Name (Anchor ref)
    deriving (Show, Eq)

data PreprocYamlToken
    = PreYamlKV YamlToken YamlToken
    | PreYamlK YamlToken -- key without specified value
    | PreYamlSeqItem YamlToken
    | PreYamlDocStart
    | PreYamlDocEnd
    | PreYamlScalar YamlToken
    | PreYamlIndent Int
    | PreYamlDedent
    | PreYamlEof
    deriving (Show, Eq)

data YamlDocument = YamlDoc [YamlValue] deriving (Show, Eq)
{-
processPreYamlTokens :: [PreprocYamlToken] -> [YamlDocument]
processPreYamlTokens [] = error "ERROR (processPreYamlTokens): empty or no-eof token list passed!"
processPreYamlTokens (PreYamlEof:_) = []
processPreYamlTokens tokens =
    let (doc, rest) =
            case tokens of
                (PreYamlDocStart:rest) -> _processPreYamlTokens_doc rest []
                otherwise -> _processPreYamlTokens_doc tokens []
    in (doc:preprocYamlTokens rest)
_processPreYamlTokens_doc :: [PreprocYamlToken] -> [YamlValue] -> (YamlDocument, [PreprocYamlToken])
_processPreYamlTokens_doc tokens doc =
    case tokens of
        (PreYamlEof:rest)      -> (doc, rest)
        (PreYamlDocEnd:rest)   -> (doc, rest)
        (PreYamlDocStart:rest) -> (doc, rest)
        _other -> _processPreYamlTokens_val tokens

_processPreYamlTokens_val :: [PreprocYamlToken] -> (YamlValue, [PreprocYamlToken])
_processPreYamlTokens_val tokens doc =
    case tokens of
        ()
-}

-- groups lexer tokens into more colpex structures like key-value pairs, sequence elements etc.
preprocYamlTokens :: [YamlToken] -> [PreprocYamlToken]
preprocYamlTokens tokens = _preprocYamlTokens $ stripRedundant tokens
_preprocYamlTokens [] = []
_preprocYamlTokens tokens =
    let (t, rest) =
            case tokens of
                (key@(SCALAR _ _):COLON:val@(SCALAR _ _):rest) -> (PreYamlKV key val, rest)
                (key@(SCALAR _ _):COLON:rest) -> (PreYamlK key, rest)
                (sc@(SCALAR _ _):rest)        -> (PreYamlScalar sc, rest)
                (DASH:sc@(SCALAR _ _):rest)   -> (PreYamlSeqItem sc, rest)
                (DOC_START:rest) -> (PreYamlDocStart, rest)
                (DOC_END:rest)   -> (PreYamlDocEnd, rest)
                (INDENT i:rest)  -> (PreYamlIndent i, rest)
                (DEDENT:rest)    -> (PreYamlDedent, rest)
                (EOF:rest)       -> (PreYamlEof, rest)
                _other -> error $ "preprocYamlTokens: Unsupported token <" ++ (show _other) ++ ">!"
    in (t:_preprocYamlTokens rest)

data YamlStructure = YamlAtom PreprocYamlToken | YamlStruct [YamlStructure] deriving (Show, Eq)

-- arranges series of yaml tokens into a structure based in indentation
extractStructure :: [PreprocYamlToken] -> YamlStructure -- Add document support
extractStructure tokens = fst $ _extractStructure tokens
_extractStructure :: [PreprocYamlToken] -> (YamlStructure, [PreprocYamlToken])
_extractStructure tokens =
    case tokens of
        (PreYamlIndent _:rest) -> let (s, rest2) = _extractStructure rest in let (YamlStruct ss, rest3) = _extractStructure rest2 in (YamlStruct (s:ss), rest3)
        (PreYamlDedent:rest) -> (YamlStruct [], rest)
        (PreYamlEof:rest) -> (YamlStruct [], (PreYamlEof:rest))
        (_other:rest)     -> let (YamlStruct st, rest2) = _extractStructure rest in (YamlStruct (YamlAtom _other:st), rest2)

-- infers type of yaml structure
inferYamlStructureType :: YamlStructure -> YamlValue
inferYamlStructureType struct = 
    case struct of
        YamlStruct (YamlAtom (PreYamlKV (SCALAR str1 style1) (SCALAR str2 style2)):rest)
            -> YamlMap $ _inferYamlStructureType_map rest [(YamlScalar str1 style1, YamlScalar str2 style2)]
        YamlStruct (YamlAtom (PreYamlK (SCALAR str style)):st@(YamlStruct _):rest)
            -> YamlMap $ _inferYamlStructureType_map rest [(YamlScalar str style, inferYamlStructureType st)]
        
        YamlStruct (YamlAtom (PreYamlSeqItem (SCALAR str style)):rest)
            -> YamlSeq $ _inferYamlStructureType_seq rest [YamlScalar str style]
        _other -> error $ "ERROR (inferStructureType): inconsistent structure type! Token: " ++ (show _other)
        
_inferYamlStructureType_map :: [YamlStructure] -> [(YamlValue, YamlValue)] -> [(YamlValue, YamlValue)]
_inferYamlStructureType_map [] vs = vs
_inferYamlStructureType_map ((YamlAtom (PreYamlKV (SCALAR str1 style1) (SCALAR str2 style2))):rest) vs
    = _inferYamlStructureType_map rest vs++[(YamlScalar str1 style1, YamlScalar str2 style2)]
_inferYamlStructureType_map (YamlAtom (PreYamlK (SCALAR str style)):st@(YamlStruct _):rest) vs
    = _inferYamlStructureType_map rest vs++[(YamlScalar str style, inferYamlStructureType st)]
_inferYamlStructureType_map _other vs
    = error $ "ERROR (_inferStructureType_map): inconsistent structure type! Token: " ++ (show _other)

_inferYamlStructureType_seq ::  [YamlStructure] -> [YamlValue] -> [YamlValue]
_inferYamlStructureType_seq [] vs = vs
_inferYamlStructureType_seq (struct:rest) vs =
    case struct of
        YamlAtom (PreYamlSeqItem (SCALAR str style))
            -> _inferYamlStructureType_seq rest vs++[YamlScalar str style]
        _other -> error $ "ERROR (_inferStructureType_seq): inconsistent structure type! Token: " ++ (show _other)


yamlToJson :: String -> String
yamlToJson yamlstr = _yamlToJson (inferYamlStructureType $ extractStructure $ preprocYamlTokens $ lexer yamlstr) 0 0
_yamlToJson :: YamlValue -> Int -> Int -> String
_yamlToJson (YamlMap kvs) ind_start ind_val = (indentStr ind_start "{\n") ++ (_yamlToJson_map kvs ind_val (ind_val + 2)) ++ "\n" ++ (indentStr ind_val "}")
_yamlToJson (YamlSeq items) ind_start ind_val = (indentStr ind_start "[\n") ++ (_yamlToJson_seq items ind_val (ind_val + 2)) ++ "\n" ++ (indentStr ind_val "]")
_yamlToJson (YamlScalar str style) ind_start ind_val = (indentStr ind_start "\"" ++ str ++ "\"")

_yamlToJson_map :: [(YamlValue, YamlValue)] -> Int -> Int -> String
_yamlToJson_map ((key, val):[]) ind_start ind_val = (_yamlToJson key ind_val (ind_val + 2)) ++ ": " ++ (_yamlToJson val 0 ind_val)
_yamlToJson_map ((key, val):rest) ind_start ind_val = (_yamlToJson key ind_val (ind_val + 2)) ++ ": " ++ (_yamlToJson val 0 ind_val) ++ ",\n" ++ (_yamlToJson_map rest ind_start ind_val)

_yamlToJson_seq :: [YamlValue] -> Int -> Int -> String
_yamlToJson_seq (item:[]) ind_start ind_val = (_yamlToJson item ind_val (ind_val + 2))
_yamlToJson_seq (item:rest) ind_start ind_val = (_yamlToJson item ind_val (ind_val + 2)) ++ ",\n" ++ (_yamlToJson_seq rest ind_start ind_val)

indentStr :: Int -> String -> String
indentStr ind str = (replicate ind ' ') ++ str

{-
yamlToJson :: String -> String
yamlToJson yaml_str = _yamlToJson $ extractStructure $ preprocYamlTokens $ lexer yaml_str
_yamlToJson :: YamlStructure -> String
_yamlToJson st =
    -}

 {-   | (next == EOF) = PreYamlEof
    | (next == DOC_START)  = PreYamlDocStart
    | (next == DOC_END)    = PreYamlDocEnd
    | (next == DEDENT)     = PreYamlDedent
    | (next == INDENT i)   = PreYamlIndent i
    | (next == DASH)       = PreYamlScalar curr
    | (next == SCALAR _ _) = PreYamlScalar curr
    | (next == COLON)      = if fst rest == SCALAR _ _ then PreYamlKV curr (fst rest)-}


stripRedundant tokens = filter (\case
                                (COMMENT _) -> False;
                                (DIRECTIVE _) -> False;
                                (SCALAR "" _) -> False; _ -> True) tokens
                            