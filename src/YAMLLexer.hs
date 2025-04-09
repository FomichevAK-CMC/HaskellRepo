module YAMLLexer (YamlToken(..), ScalarStyle(..), lexer) where

import Data.Char (isSpace, isDigit, isAlphaNum)
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

lstrip :: String -> String
lstrip = dropWhile isSpace
isAnchorChar :: Char -> Bool
isAnchorChar c = isAlphaNum c || c == '_' || c == '-'
rtrim :: String -> String
rtrim = reverse . dropWhile isSpace . reverse

-- Main lexer function
lexer :: String -> [YamlToken]
lexer input = processLines (lines input) [0]

-- processLines function
processLines :: [String] -> [Int] -> [YamlToken]
processLines [] stack = replicate (length stack - 1) DEDENT ++ [EOF]
processLines (line:ls) stack@(currentIndent:_) =
    let (leadingSpaces, restOfLine) = span isSpace line
        indent = length leadingSpaces
        isLineEffectivelyBlank = all isSpace restOfLine
        isLineCommentOnly = case lstrip restOfLine of ('#':_) -> True; _ -> False
    in
    if isLineEffectivelyBlank then processLines ls stack
    else if isLineCommentOnly then
        tokenize (lstrip restOfLine) ++ processLines ls stack
    else
        if indent > currentIndent then
            let tokens = tokenize restOfLine
            in INDENT indent : tokens ++ processLines ls (indent:stack)
        else if indent < currentIndent then
            let (dedents, newStack) = popIndents indent stack
            in if null newStack then
                   ERROR ("Invalid dedent to column " ++ show indent) : processLines ls stack
               else
                   let tokens = tokenize restOfLine
                   in dedents ++ tokens ++ processLines ls newStack
        else
            let tokens = tokenize restOfLine
            in tokens ++ processLines ls stack

-- popIndents function
popIndents :: Int -> [Int] -> ([YamlToken], [Int])
popIndents targetIndent stack@(current:rest)
    | current == targetIndent = ([], stack)
    | current > targetIndent && not (null rest) = let (dedents, finalStack) = popIndents targetIndent rest  in (DEDENT : dedents, finalStack)
    | otherwise = ([], [])


-- tokenize function and its where clause helpers
tokenize :: String -> [YamlToken]
tokenize lineContent = _tokenize (lstrip lineContent) -- Strip leading inline space ONCE
  where
    _tokenize :: String -> [YamlToken]
    _tokenize "" = []
    _tokenize input =
        case input of
            s | "---" `isPrefixOf` s && followedBySepOrEnd 3 s -> DOC_START : _tokenize (lstrip (drop 3 s))
            s | "..." `isPrefixOf` s && followedBySepOrEnd 3 s -> DOC_END   : _tokenize (lstrip (drop 3 s))
            '#':cs -> [COMMENT ('#':cs)]
            '%':cs -> let (directive, _) = span (/= '\n') cs in [DIRECTIVE ('%':directive)]
            ':':cs | isSep cs -> COLON    : _tokenize (lstrip cs)
            '-':cs | isSep cs -> DASH     : _tokenize (lstrip cs)
            '?':cs | isSep cs -> QUESTION : _tokenize (lstrip cs)
            '|':cs | isSep cs -> BLOCK_LITERAL : _tokenize (lstrip cs) -- Keep as indicator
            '>':cs | isSep cs -> BLOCK_FOLDED  : _tokenize (lstrip cs) -- Keep as indicator
            '[':cs -> LBRACKET : _tokenize cs
            ']':cs -> RBRACKET : _tokenize cs
            '{':cs -> LBRACE   : _tokenize cs
            '}':cs -> RBRACE   : _tokenize cs
            ',':cs -> COMMA    : _tokenize cs
            '&':cs -> lexAnchor ANCHOR cs
            '*':cs -> lexAnchor ALIAS cs
            '!':cs -> lexTag ('!':cs)
            -- Quoted Scalars
            '\'':cs -> lexSingleQuoted cs
            '"':cs -> lexDoubleQuoted cs
            -- Default: Plain Scalar
            _ -> lexPlainScalar input

    isSep :: String -> Bool
    isSep "" = True; isSep "\r" = True; isSep (' ':_) = True; isSep ('#':_) = True; isSep _ = False; -- (INVESTIGATE) after (lines) function there shouldn't be any \r but they are still there. 
    followedBySepOrEnd :: Int -> String -> Bool
    followedBySepOrEnd i s | length s <= i = True | otherwise = let c = s !! i in isSpace c || c == '#'
    lexAnchor :: (String -> YamlToken) -> String -> [YamlToken]
    lexAnchor tokenConstructor cs = let (name, rest) = span isAnchorChar cs in if null name then ERROR "Invalid anchor/alias: missing name after indicator" : _tokenize rest else tokenConstructor name : _tokenize rest
    lexTag :: String -> [YamlToken]
    lexTag ('!':cs) = let (tagChars, rest) = span (\c -> not (isSpace c) && c `notElem` ",[]{}") cs in TAG ('!':tagChars) : _tokenize rest
    lexTag _ = [ERROR "Invalid call to lexTag"]

    -- Lex single-quoted scalar
    lexSingleQuoted :: String -> [YamlToken]
    lexSingleQuoted input =
        let (scalarAttempt, rest, terminated) = consumeSingle "" input
        in if terminated
           then SCALAR scalarAttempt SingleQuotedStyle : _tokenize rest
           else [ERROR ("Unterminated single-quoted string starting with: '" ++ take 30 scalarAttempt ++ "...")]

    -- Lex double-quoted scalar
    lexDoubleQuoted :: String -> [YamlToken]
    lexDoubleQuoted input =
        let (scalarAttempt, rest, terminated) = consumeDouble "" input
        in if terminated
           then SCALAR scalarAttempt DoubleQuotedStyle : _tokenize rest
           else [ERROR ("Unterminated double-quoted string starting with: \"" ++ take 30 scalarAttempt ++ "...")]

    -- Lex plain scalar
    lexPlainScalar :: String -> [YamlToken]
    lexPlainScalar input =
        let (scalarVal, rest) = consumePlain "" input
        in if null scalarVal then
               if null rest then [] else _tokenize rest
           else SCALAR (rtrim scalarVal) PlainStyle : _tokenize rest

    consumeSingle :: String -> String -> (String, String, Bool)
    consumeSingle acc "" = (acc, "", False); consumeSingle acc ('\'':'\'':xs) = consumeSingle (acc ++ "'") xs; consumeSingle acc ('\'':xs) = (acc, xs, True); consumeSingle acc (x:xs) = consumeSingle (acc ++ [x]) xs
    consumeDouble :: String -> String -> (String, String, Bool)
    consumeDouble acc "" = (acc, "", False); consumeDouble acc ('\\':'"':xs) = consumeDouble (acc ++ "\"") xs; consumeDouble acc ('\\':'\\':xs) = consumeDouble (acc ++ "\\") xs; consumeDouble acc ('\\':'n':xs) = consumeDouble (acc ++ "\n") xs; consumeDouble acc ('"':xs) = (acc, xs, True); consumeDouble acc ('\\':x:xs)= consumeDouble (acc ++ ['\\', x]) xs; consumeDouble acc (x:xs) = consumeDouble (acc ++ [x]) xs

    -- Returns (AccumulatedString, Remainder)
    consumePlain :: String -> String -> (String, String)
    consumePlain acc "" = (acc, "") -- End of input string
    consumePlain acc remaining@(c:cs)
        | c == '#' = (acc, remaining)
        -- Check structural indicators THAT REQUIRE a following separator
        | c == ':' && isSep cs = (acc, remaining)
        | c == '-' && isSep cs = (acc, remaining)
        | c == '?' && isSep cs = (acc, remaining)
        | c == '|' && isSep cs = (acc, remaining)
        | c == '>' && isSep cs = (acc, remaining)
        -- Check flow indicators/comma (don't require space)
        | c `elem` "[]{}," = (acc, remaining) -- Stop *before* flow chars
        -- Check potential start of document markers (less likely mid-scalar, but for safety)
        | c == '-' && isPrefixOf "--" cs && followedBySepOrEnd 3 remaining = (acc, remaining) -- Stop before ---
        | c == '.' && isPrefixOf ".." cs && followedBySepOrEnd 3 remaining = (acc, remaining) -- Stop before ...
        -- Check anchor/alias/tag starts (cannot be inside plain scalar)
        | c `elem` "&*!" = (acc, remaining) -- Stop before these indicators
        -- If none of the above stopping conditions match, consume 'c' and continue
        | otherwise = consumePlain (acc ++ [c]) cs