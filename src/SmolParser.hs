{-# LANGUAGE InstanceSigs #-}

module SmolParser  where

import Control.Applicative

newtype Parser a = 
    Parser {runParser :: String -> Maybe (a, String)} 

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
        Parser $ \str -> Just(a, str)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    p1 <*> p2 =
        Parser $ \str -> do
            (f, rest)  <- runParser p1 str
            (a, rest') <- runParser p2 rest
            return ((f a), rest')
            -- case runParser p1 str of
                -- Just(f, rest) -> 
                --     case runParser p2 rest of 
                --     Just(a, rest') -> Just((f a), rest')
                --     Nothing        -> Nothing
                -- Nothing        -> Nothing

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b -- [String -> Maybe b]
    p >>= f = 
        Parser $ \str ->
            do 
                (a, rest) <- runParser p str
                runParser (f a) rest
        -- Parser $ 
        --     \string ->
        --         case runParser p string of
        --             Just(a, rest) -> 
        --                 runParser (f a) rest
        --             Nothing       -> Nothing

instance Alternative Parser where
    -- The Parser that Always Fails
    empty :: Parser a
    empty = Parser $ \_ -> Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = 
        Parser $ \ str ->
            case runParser p1 str of
                Just (a, str')    -> Just (a, str')
                Nothing ->
                    runParser p2 str

run :: Parser a -> String -> Maybe a
run ps str = do 
    (a,_)  <- runParser ps str
    return a


-- Like many, but fails if no elements are present
repeat :: Parser a -> Parser [a]
repeat p = Parser $ \ str ->
  case runParser (many p) str of
    Just ([], str)  -> Nothing
    Just (a)   -> Just (a)

-- Matches true if element end of string
-- else Nothing
eof :: Parser ()
eof = Parser $ \ str ->
    if str == []
        then Just ((),[])
        else Nothing


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
                    Just(lit, last)
                else 
                    Nothing

digit :: Parser Char
digit =
    Parser $ \str -> 
        case str of 
            []  ->  Nothing
            (c:cs) ->
                if c >= '0' && c <= '9'
                then Just (c, cs)
                else Nothing

hexdigit :: Parser Char
hexdigit =
    Parser $ \ str ->
        case str of 
            [] -> Nothing
            (c:cs) ->
                if  c >= '0' && c <= '9' 
                    || c >= 'a' && c <= 'f' 
                    || c >= 'A' && c <= 'F'
                    then Just (c, cs)
                    else Nothing  


number :: Parser String
number = SmolParser.repeat digit

hexnumber :: Parser String
hexnumber = SmolParser.repeat hexdigit

char :: Char -> Parser Char
char g = 
    Parser $ (\(c:cs) -> if g == c then Just(c, cs) else Nothing)

-- parse a string with n elements
string :: Int -> Parser String
string n = 
    Parser $ \str ->
        let (hd, tl) = splitAt n str in
            if length hd == n
                then  Just (hd,tl)
                else Nothing

whitespace :: Parser ()
whitespace = 
    Parser $ \(c:cs) -> 
        case c of
            ' '  -> Just ((), cs)
            '\n' -> Just ((), cs)
            '\t' -> Just ((), cs)
            _    -> Nothing

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
