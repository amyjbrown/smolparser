module JsonParse(
    JsonValue,
    printResult,
    opeAndParseFile,
    jsValue,
    jsObject,
    jsArray,
    jsNumber,
    jsString,
    jsBool,
    jsNil
    ) where

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
\ \"key3\": nil}"

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
-- OK so the basic syntax is
-- Object
--  { ws }
--  {members}
-- members
--  member ("," member)*
-- Member
--  string ":" value
    noMember <|> members

noMember :: Parser JsonValue
noMember = do
    char '{'
    skipws
    char '}'
    return . JsonObject . Map.fromList $ []

members :: Parser JsonValue
members = do
    char '{'
    skipws
    first <- member
    rest <- some getRest
    skipws
    char '}'
    return . JsonObject . Map.fromList $ first:rest

    where
        getRest :: Parser (JsonValue, String)
        getRest = do
            skipws
            literal ','

member :: Parser (String, JsonValue)
member = do
    string <- jsString
    skipws
    char ':'
    skipws
    value <- jsValue
    return (extractString string, value)

    where 
        extractString :: JsonValue -> String
        extractString (JsonString s) = s
        -- extractString x = error "extractString passed non-JsonString value: " ++ (show x)

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