{-# LANGUAGE InstanceSigs #-}

module SmolParser where

import Control.Applicative
import Data.Char

newtype Parser a = 
    Parser {runParser :: String -> Either ErrorMessage (a, String)} 

type ErrorMessage = String

instance Functor Parser where
    fmap :: (a->b) -> Parser a -> Parser b
    fmap f p = Parser $ \ str ->
        do 
            (a,str') <- runParser p str
            return ((f a), str')
        -- Parser $ \string -> case runParser ps string of
        --             Just (a, rest) -> Just( (f a), rest)
        --             Nothing           -> Nothing

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = 
        Parser $ \str -> Right (a, str)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    p1 <*> p2 =
        Parser $ \str -> do
            (f, rest)  <- runParser p1 str
            (a, rest') <- runParser p2 rest
            return ((f a), rest')

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b -- [String -> Maybe b]
    p >>= f = 
        Parser $ \str ->
            case runParser p str of 
                Right (a, rest) ->
                    runParser (f a) rest
                Left error      ->
                    Left error
            -- do 
            --     (a, rest) <- runParser p str
            --     runParser (f a) rest
            --     -- TODO - fix this shit

instance Alternative Parser where
    -- The Parser that Always Fails
    empty :: Parser a
    empty = Parser $ \_ -> Left "[empty] gauranteed failre"

    (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = 
        Parser $ \ str ->
            case runParser p1 str of
                Right (a, str')    -> Right (a, str')
                Left err ->
                    case runParser p2 str of
                        Right (a, str'')  -> Right (a, str'')
                        Left err'         -> Left $ "(<|>) failed:\n\t" ++ err ++ "\nthen\n\t" ++ err'


instance Show (Parser a) where
    show _ = "<parser>"


errorString :: String -> String
errorString str = 
    let l = length str in
        if l > 25 then
            "'" ++ (take 25 str ++ "...") ++ "'"
            else '\'':str ++ "'"

run :: Parser a -> String -> Either ErrorMessage a
run ps str = do 
    (a,_)  <- runParser ps str
    return a

-- Optionally consumes a token, and if the parser fails, return first arg
option :: a -> Parser a -> Parser a
option x p = 
    Parser $ \ str -> 
        case str of
            [] -> Right (x,[])
            _ -> case runParser p str of
                Right (a, rest) ->  Right (a, rest)
                Left _         ->   Right (x, str)


optional :: Parser a -> Parser ()
optional p = Parser $ \ str ->
    case str of 
        ""  -> Right ((), str)
        _   ->
            case runParser p str of
                Right (_, rest)   -> Right ((), rest)
                Left _            -> Right ((),  str)


choice :: [Parser a] -> Parser a
choice a = foldr1 (<|>) a

-- Like many, but fails if no elements are present
repeat :: Parser a -> Parser [a]
repeat p = Parser $ \ str ->
  case runParser (many p) str of
    Right ([], str)    ->  Left $ "repeat failed to show as many arguements as necessary at: " ++ (errorString str)
    Right (a)        ->  Right (a)

-- Matches true if element end of string
-- else Nothing
eof :: Parser ()
eof = Parser $ \ str ->
    if str == []
        then Right ((),[])
        else Left "Failed to match end of file"


-- matches and returns the string if it matches,
-- else it fails
-- e.g. run (litreal ":") ":quit" = ":" 
literal :: String -> Parser String
literal lit = 
    Parser $ \str ->
        let n = length lit 
            (first, last) = splitAt n str
            in
                if first == lit then 
                    Right (lit, last)
                else 
                    Left $ "[literal] failed to match literal " ++ lit ++ "at: " ++ (errorString str)

digit :: Parser Char
digit =
    Parser $ \str -> 
        case str of 
            []  ->  Left "[digit] end of file reached"
            (c:cs) ->
                if c >= '0' && c <= '9'
                then Right (c, cs)
                else Left $ "failed to match digit at " ++ (show  c) ++ "(" ++ (show $ ord c) ++ ")" ++ " at: " ++ (errorString str)

hexdigit :: Parser Char
hexdigit =
    Parser $ \ str ->
        case str of 
            [] -> Left "[hexdigit] end of file reached"
            (c:cs) ->
                if  c >= '0' && c <= '9' 
                    || c >= 'a' && c <= 'f' 
                    || c >= 'A' && c <= 'F'
                    then Right (c, cs)
                    else Left $ "[hexdigit] Couldn't match char " ++ (show c) ++ "at: " ++ (errorString str) 


-- Return all char's conseuctively fulfil the predicate
-- Will return empty string if no characters found
-- example:
--  run (munch $ upperCase) "ABCdeFG" = "ABC"
--  run (munch $ upperCase) "0123"    = ""
munch :: (Char -> Bool) -> Parser String
munch f = 
    Parser $ \ str ->
        case _munch f (str, "") of
            (rest, result)  -> Right (result, rest)

-- Like munch but will return nothing if 0 following characters fit the predicate
munch1 :: (Char -> Bool) -> Parser String
munch1 f = 
    Parser $ \ str ->
        case _munch f (str, "") of
            ("", str)         -> Left $ "[munch1] failed to munch at:\n\t" ++ (errorString str)
            (rest, result)  -> Right (result, rest)
    
    where

-- REMEMBER, (input, result) !!! 
_munch :: (Char->Bool) -> (String, String) -> (String, String)
_munch _ ([], a) = ([], a)

_munch f (c:cs,s2) =
    if f c
        then _munch f (cs, s2 ++ [c])
        else (c:cs, s2) -- stop iteration otherwise

_munch _ ([], a) = ([], a)

number :: Parser String
number = SmolParser.repeat digit

hexnumber :: Parser String
hexnumber = SmolParser.repeat hexdigit

char :: Char -> Parser Char
char g = 
    Parser $ \str -> 
        case str of 
            (c:cs) ->
                if g == c 
                    then Right (c, cs) 
                    else Left $ bigOlError (c, g) str
            []     -> 
                Left $ "[char] reached end of file " 

    where
        bigOlError :: (Char, Char) -> String -> String
        bigOlError (actual, expected) str = 
            "[char] expected '" ++ [expected] ++ "'(" ++ (show . ord $ expected) ++ ") "
            ++ "recieved '" ++ [actual] ++ "'(" ++ (show . ord $ actual) ++ ") at:\n\t" ++ (errorString str)



-- parse a string with n elements
string :: Int -> Parser String
string n = 
    Parser $ \str ->
        let (hd, tl) = splitAt n str in
            if length hd == n
                then  Right (hd,tl)
                else Left $ "Failed to match " ++ (show n) ++ "chars at:\n\t" ++ (errorString str)

skipws :: Parser ()
skipws = SmolParser.optional $ many whitespace

whitespace :: Parser ()
whitespace = 
    Parser $ \(c:cs) -> 
        case c of
            ' '  -> Right ((), cs)
            '\n' -> Right ((), cs)
            '\t' -> Right ((), cs)
            '\f' -> Right ((), cs)
            '\v' -> Right ((), cs)
            '\r' -> Right ((), cs)
            _    -> Left $ "Failed to match whitespace char with " ++ (show c) ++ "(" ++ (show . ord $ c) ++ ")" ++ "at:\n\t:" ++ (errorString $ c:cs)



follows :: Parser a -> Parser b -> Parser (a,b)
psa `follows` psb = 
    Parser $ \str -> do
        (a, rest)  <- runParser psa str 
        (b, rest') <- runParser psb rest
        return ((a,b), rest')
        -- case runParser psa string of
        --     Just(a, rest) ->
        --         case runParser psb rest of
        --             Just(b, rest') -> Just((a,b), rest')
        --             Nothing        -> Nothing
        --     Nothing -> Nothing

discard :: Parser a -> Parser ()
discard ps = do
        ps
        return ()