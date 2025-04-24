module YAMLLexer (YamlToken(..), ScalarStyle(..), lexer) where
import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- different styles a scalar can have
-- will be used to infer types
data ScalarStyle
    = PlainStyle
    | SingleQuotedStyle
    | DoubleQuotedStyle
    deriving (Show, Eq)

-- Define the token types
data YamlToken
    -- Structural
    = DOC_START        -- ---
    | DOC_END          -- ...
    | COLON            -- : (followed by space or newline/comment)
    | DASH             -- - (followed by space or newline/comment)
    | LBRACKET         -- [
    | RBRACKET         -- ]
    | LBRACE           -- {
    | RBRACE           -- }
    | COMMA            -- ,
    | QUESTION         -- ? (followed by space or newline/comment)
    -- Indentation (explicit level)
    | INDENT Int
    | DEDENT
    -- Data
    | SCALAR String ScalarStyle -- Store scalar style along with value
    -- Indicators for block scalars (parser needs to handle content)
    | BLOCK_LITERAL    -- | (followed by space or newline/comment)
    | BLOCK_FOLDED     -- > (followed by space or newline/comment)
    | ANCHOR String    -- &name
    | ALIAS String     -- *name
    | TAG String       -- !tag
    -- Meta
    | DIRECTIVE String -- %YAML, %TAG
    | COMMENT String   -- # comment text
    | EOF              -- End of input
    | ERROR String     -- Lexing error
    deriving (Show, Eq)


rtrim :: String -> String
rtrim = reverse . dropWhile isSpace . reverse

-- Main lexer function
lexer :: String -> [(Int, YamlToken)]
lexer input = processLines (lines input)

processLines :: [String] -> [(Int, YamlToken)]
processLines [] = [(0, EOF)]
processLines (line:ls) =
    let (leadingSpaces, restOfLine) = span isSpace line
        indent = length leadingSpaces
        isLineEffectivelyBlank = all isSpace restOfLine
    in
    if isLineEffectivelyBlank then
        processLines ls
    else
        -- Tokenize the line content, passing the line's starting indent
        let lineTokens = tokenize restOfLine indent
        in lineTokens ++ processLines ls



-- tokenize function and its where clause helpers
tokenize :: String -> Int -> [(Int, YamlToken)]
tokenize ('#':cs) ind = [(ind, COMMENT ('#':cs))]
tokenize lineContent ind = _tokenize lineContent ind
  where
    _tokenize :: String -> Int -> [(Int, YamlToken)]
    _tokenize "" _ = []
    _tokenize input indent =
        case input of
            '#':cs -> [(indent, COMMENT ('#':cs))]
            ' ':cs -> _tokenize cs (indent + 1)
            '-':'-':'-':cs | isSep cs -> (indent, DOC_START) : _tokenize cs (indent + 3)
            '.':'.':'.':cs | isSep cs -> (indent, DOC_END)   : _tokenize cs (indent + 3)
            '%':cs -> let (directive, _) = span (/= '\n') cs in
                          [(indent, DIRECTIVE ('%':directive))]
            ':':cs | isSep cs -> (indent, COLON)    : _tokenize cs (indent + 1)
            '-':cs | isSep cs -> (indent, DASH)     : _tokenize cs (indent + 1)
            '?':cs | isSep cs -> (indent, QUESTION) : _tokenize cs (indent + 1)
            '|':cs | isSep cs -> (indent, BLOCK_LITERAL) : _tokenize cs (indent + 1)
            '>':cs | isSep cs -> (indent, BLOCK_FOLDED)  : _tokenize cs (indent + 1)
            '[':cs -> (indent, LBRACKET) : _tokenize cs (indent + 1)
            ']':cs -> (indent, RBRACKET) : _tokenize cs (indent + 1)
            '{':cs -> (indent, LBRACE)   : _tokenize cs (indent + 1)
            '}':cs -> (indent, RBRACE)   : _tokenize cs (indent + 1)
            ',':cs -> (indent, COMMA)    : _tokenize cs (indent + 1)
            -- Quoted Scalars
            '\'':cs -> lexSingleQuoted cs indent
            '"':cs -> lexDoubleQuoted cs indent
            -- Default: Plain Scalar
            _ -> lexPlainScalar input indent

    

    isSep :: String -> Bool
    isSep "" = True; isSep "\r" = True; isSep (' ':_) = True; isSep ('#':_) = True; isSep _ = False;
    followedBySepOrEnd :: Int -> String -> Bool
    followedBySepOrEnd i s | length s <= i = True
                           | otherwise = let c = s !! i in isSpace c || c == '#'

    -- Lex single-quoted scalar
    lexSingleQuoted :: String -> Int -> [(Int, YamlToken)]
    lexSingleQuoted input indent =
        let (scalarAttempt, rest, terminated) = consumeSingle "" input
            len = length scalarAttempt
        in if terminated
           then (indent, SCALAR scalarAttempt SingleQuotedStyle) : _tokenize rest (indent + len + 1)
           else error $ "Unterminated single-quoted string starting with: '"
                        ++ take 30 scalarAttempt ++ "..."

    -- Lex double-quoted scalar
    lexDoubleQuoted :: String -> Int -> [(Int, YamlToken)]
    lexDoubleQuoted input indent =
        let (scalarAttempt, rest, terminated) = consumeDouble "" input
            len = length scalarAttempt
        in if terminated
           then (indent, SCALAR scalarAttempt DoubleQuotedStyle):(_tokenize rest (indent + len + 1))
           else error $ "Unterminated double-quoted string starting with: \""
                        ++ take 30 scalarAttempt ++ "..."

    -- Lex plain scalar
    lexPlainScalar :: String -> Int -> [(Int, YamlToken)]
    lexPlainScalar input indent =
        let (scalarVal, rest) = consumePlain "" input
            val = rtrim scalarVal
        in if null scalarVal then
               if null rest then [] else (_tokenize rest indent)
           else (indent, SCALAR val PlainStyle) : _tokenize rest (indent + (length val))

    consumeSingle :: String -> String -> (String, String, Bool)
    consumeSingle acc "" = (acc, "", False)
    consumeSingle acc ('\'':'\'':xs) = consumeSingle (acc ++ "'") xs
    consumeSingle acc ('\'':xs) = (acc, xs, True)
    consumeSingle acc (x:xs) = consumeSingle (acc ++ [x]) xs
    
    consumeDouble :: String -> String -> (String, String, Bool)
    consumeDouble acc "" = (acc, "", False)
    consumeDouble acc ('\\':'"':xs) = consumeDouble (acc ++ "\"") xs
    consumeDouble acc ('\\':'\\':xs) = consumeDouble (acc ++ "\\") xs
    consumeDouble acc ('\\':'n':xs) = consumeDouble (acc ++ "\n") xs
    consumeDouble acc ('"':xs) = (acc, xs, True)
    consumeDouble acc ('\\':x:xs) = consumeDouble (acc ++ ['\\', x]) xs
    consumeDouble acc (x:xs) = consumeDouble (acc ++ [x]) xs

    -- Returns (AccumulatedString, Remainder)
    consumePlain :: String -> String -> (String, String)
    consumePlain acc "" = (acc, "") -- End of input string
    consumePlain acc (' ':'#':cs) = ((rtrim acc), '#':cs)
    consumePlain acc remaining@(c:cs)
        -- Check structural indicators THAT REQUIRE a following separator
        | c == ':' && isSep cs = (acc, remaining) -- Stop *before* ': ' etc.
        | c == '-' && isSep cs = (acc, remaining) -- Stop *before* '- ' etc.
        -- Check flow indicators/comma (don't require space)
        | c `elem` "[]{}," = (acc, remaining) -- Stop *before* flow chars
        -- Check potential start of document markers (less likely mid-scalar, but for safety)
        | c == '-' && isPrefixOf "--" cs && followedBySepOrEnd 3 remaining = (acc, remaining)
        | c == '.' && isPrefixOf ".." cs && followedBySepOrEnd 3 remaining = (acc, remaining)
        | otherwise = consumePlain (acc ++ [c]) cs