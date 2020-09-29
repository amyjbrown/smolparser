import SmolParser
import Control.Applicative
import qualified Data.Map.Strict as Map
import System.IO

data JsonValue = 
    JsonObject (Map.Map String JsonValue)
    | JsonArray [JsonValue]
    | JsonNumber Double
    | JsonString String
    | JsonBool Bool
    | JsonNil
    deriving Show


-- -- todo create top level json parser
-- may replace later


printResult :: String -> IO ()
printResult path = do
    result <- opeAndParseFile path
    putStrLn $ show result
    return ()


opeAndParseFile :: String -> IO JsonValue
opeAndParseFile path = do
    contents <- System.IO.readFile path
    case run parseFile contents of
        Just a    -> return a
        Nothing   -> fail "File was unable to load!"



parseFile :: Parser JsonValue
parseFile = jsObject <|> jsArray


jsObject :: Parser JsonValue
jsObject = do
    char '{'
    skipws
    (value, values) <- keyValue `follows` many (char ',' *> keyValue)
    skipws
    char '}'
    return $ JsonObject $ Map.fromList (value:values)
    where 
        keyValue :: Parser (String, JsonValue)
        keyValue = do
            skipws
            key <- jsString
            skipws
            char ':'
            skipws
            value <- jsValue
            return (extractString key, value)
        
        extractString :: JsonValue -> String
        extractString (JsonString s) = s
        extractString _              = error "[jsObject()] Attempted to extract string form non JsonString object"


jsArray :: Parser JsonValue -- todo JsonArray
jsArray = do
    char '['
    skipws
    (value, values) <-  jsValue `follows` many jsValue'
    skipws
    char ']'
    return $ JsonArray $ value:values
    where 
        jsValue' :: Parser JsonValue
        jsValue' = do
            skipws
            char ','
            skipws
            value <- jsValue
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

jsString :: Parser JsonValue
jsString = do
    char '\"'
    body <- munch (\c -> c /= '\"')
    char '\"'
    return $ JsonString $ body

jsNumber :: Parser JsonValue
jsNumber = do
    intPart      <- SmolParser.number
    realPart     <-  option ".0" (SmolParser.literal "." *> SmolParser.number)
    exponentPart <- option "" jsExponent
    return $ JsonNumber $ read $ intPart ++ "." ++ realPart ++ exponentPart

jsExponent :: Parser String
jsExponent = do
    leading <-literal "e" <|> literal "E"
    signum  <- literal "+" <|> literal "-" <|> literal ""
    nums    <- number
    return $ leading ++ signum ++ nums