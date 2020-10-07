import SmolParser
import Control.Applicative
import qualified Data.Map.Strict as Map
import System.IO
import Data.Char

data JsonValue = 
    JsonObject (Map.Map String JsonValue)
    | JsonArray [JsonValue]
    | JsonNumber Double
    | JsonString String
    | JsonBool Bool
    | JsonNil
    deriving (Show, Eq)


-- todo create top level json parser
-- may replace later
t1 :: String
t1  = "{\"Key\": \"Value\",\
\ \"key2\": 1, \
\ \"key3\": nil"

t2 = "[1, 2, 3]"



printResult :: String -> IO ()
printResult path = do
    result <- opeAndParseFile path
    print result
    return ()


opeAndParseFile :: String -> IO JsonValue
opeAndParseFile path = do
    contents <- System.IO.readFile path
    case run parseFile contents of
        Just a    -> return a
        Nothing   -> fail "Parse Error in File!"



parseFile :: Parser JsonValue
parseFile = jsObject <|> jsArray


jsObject :: Parser JsonValue
jsObject = do
    skipws
    char '{'
    skipws
    pairs <- option [] getPairs
    skipws
    char '}'
    return $ JsonObject (Map.fromList pairs)

    where
        getPairs :: Parser [(String, JsonValue)]
        getPairs = do
            kv  <- keyValue
            skipws
            kvs <- many $ char ',' *> skipws *> keyValue
            return $ kv:kvs

        keyValue :: Parser (String, JsonValue)
        keyValue = do
            key <- jsString
            skipws
            char ':'
            skipws
            value <- jsValue
            skipws
            char ','
            return (extractString key, value)
        
        extractString :: JsonValue -> String
        extractString (JsonString s) = s
        extractString _              = error "[jsObject()] Attempted to extract string form non JsonString object"


jsArray :: Parser JsonValue -- todo JsonArray
jsArray = do
    skipws
    char '['
    skipws
    values <-  option [] getValues
    skipws
    char ']'
    return $ JsonArray (values)
    where 
        getValues :: Parser [JsonValue]
        getValues = do
            v <- jsValue
            vs <- many jsValue'
            return $ v:vs

        jsValue' :: Parser JsonValue
        jsValue' = do
            skipws
            char ','
            skipws
            value <- jsValue
            skipws
            return value


jsValue :: Parser JsonValue
jsValue = jsObject <|> jsArray <|> jsBool <|> jsNil <|> jsNumber <|> jsString

jsBool :: Parser JsonValue
jsBool = do
    f <- literal "true" <|> literal "false"
    return $ JsonBool $ f == "true"  -- for when I refactor it to map onto JsonBool

jsNil :: Parser JsonValue
jsNil = do
    literal "nil"
    return JsonNil
-- TODO on jsString - Handle escapes
-- Unfortunately I'd have to switch to `Text` for handling unicode, not sure I want to handle that atm
jsString :: Parser JsonValue
jsString = do
    char '\"'
    body <- characters
    char '\"'
    return $ JsonString $ body

-- characters :: Parser [String]
-- characters = some character

characters :: Parser String
characters = do
    glyphs <- some character
    return glyphs

character :: Parser Char
character = 
    regular <|> escapes
    where 
    regular = Parser $ \ str ->
        case str of
            ""       -> Nothing
            '\"':_   -> Nothing
            '\\':_   -> Nothing
            c:rest   -> Just (c, rest) 

escapes :: Parser Char
escapes = 
    choice [
        literal "\\\"" >> return '\"',
        literal "\\\\" >> return '\\',
        literal "\\/"  >> return '/',
        literal "\\b"  >> return '\b',
        literal "\\f"  >> return '\f',
        literal "\\n"  >> return '\n',
        literal "\\r"  >> return '\r',
        literal "\\t"  >> return '\t'
        -- TODO hex codes   
    ]

jsNumber :: Parser JsonValue
jsNumber = do
    intPart      <- SmolParser.number
    realPart     <-  option "0" (SmolParser.literal "." *> SmolParser.number)
    exponentPart <- option "" jsExponent
    return $ JsonNumber $ read (intPart ++ "." ++ realPart ++ exponentPart)

jsExponent :: Parser String
jsExponent = do
    leading <-literal "e" <|> literal "E"
    signum  <- literal "+" <|> literal "-" <|> literal ""
    nums    <- number
    return $ leading ++ signum ++ nums