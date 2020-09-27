import SmolParser
import Control.Applicative
import qualified Data.Map.Strict as Map

data JsonValue = 
    JsonMap (Map.Map String JsonValue)
    | JsonArray [JsonValue]
    | JsonNumber Double
    | JsonString String
    | JsonBool Bool
    | JsonNil 


-- -- todo create top level json parser
-- may replace later
skipws :: Parser a -> Parser b -> Parser (a,b)
skipws p1 p2 = do
    a <- p1
    whitespace
    b <- p2
    return (a,b)


jsObject :: Parser (Map.Map String String)
jsObject = do
    char '{'
    whitespace
    (value, values) <- keyValue `skipws` many keyValue
    whitespace
    char '}'
    return $ Map.fromList (value:values)
    where 
        keyValue :: Parser (String, String)
        keyValue = do
            key <- jsString
            whitespace
            char ':'
            whitespace
            value <- jsValue
            whitespace
            char ','
            return (key, value)


jsArray :: Parser [String] -- todo JsonArray
jsArray = do
    char '['
    whitespace
    (value, values) <- jsValue `skipws` many (jsValue <* whitespace <* char ',')
    whitespace
    char ']'
    return (value:values)

jsValue :: Parser String
jsValue = jsBool <|> jsNil <|> jsNumber <|> jsString

jsBool :: Parser String
jsBool = do
    f <- literal "true" <|> literal "false"
    return f -- for when I refactor it to map onto JsonBool

jsNil :: Parser String
jsNil = do
    literal "nil"

jsString :: Parser String
jsString = do
    char '\"'
    body <- munch (\c -> c /= '\"')
    char '\"'
    return body

jsNumber :: Parser String
jsNumber = do
    intPart      <- SmolParser.number
    realPart     <-  option "" (SmolParser.literal "." *> SmolParser.number)
    exponentPart <- option "" jsExponent
    return $ intPart ++ "." ++ realPart ++ exponentPart

jsExponent :: Parser String
jsExponent = do
    leading <-literal "e" <|> literal "E"
    signum  <- literal "+" <|> literal "-" <|> literal ""
    nums    <- number
    return $ leading ++ signum ++ nums