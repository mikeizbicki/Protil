module Parser
    ( parseText
    , parseQuery
    , parse
    ) where

import Data.Char
import Data.Monoid
import Text.ParserCombinators.Parsec

import DataTypes
import Logic
-- import Truths.Boolean

-- Parsing below

-- parseString :: Parser String
-- parseString = 
--         parseStringDelim '"'
--     <|> parseStringDelim '\''
-- 
-- parseStringDelim :: Char -> Parser String
-- parseStringDelim delim = do 
--     char delim
--     x <- many (noneOf [delim])
--     char delim
--     return $ x 

atomSymbol :: Parser Char
atomSymbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser String
parseString = do 
    first <- letter <|> atomSymbol
    rest <- many (letter <|> digit <|> atomSymbol)
    return $ first:rest
    
isVar :: String -> Bool
isVar (x:xs)
    | x == '_'  = True
    | isUpper(x)= True
    | otherwise = False

parseAtom :: Parser Term
parseAtom = do
    str <- parseString
    return $ if isVar str 
                then Var str
                else Atom str
    
parseTerm :: Parser Term
parseTerm = do
    functor <- parseString
    spaces
    args <- option [] $ do
        char '('
        args <- option [] $ do
            args <- parseList parseAtom
            return args
        char ')'
        return $ args
    return $ CTerm (functor) args

parseList :: Parser Term -> Parser [Term]
parseList parser = do
    first <- parser
    next <- option [] $ do
        spaces
        char ','
        spaces
        next <- parseList parser
        return next
    return $ first:next
    
parseRule :: String -> Parser Rule
parseRule controlStr = try (parseRuleComplex controlStr) <|> (parseRuleSimple controlStr)
    
parseRuleComplex :: String -> Parser Rule
parseRuleComplex controlStr = do
    head <- parseTerm
    spaces
    char ':'
--     truthVal <- option defaultTruthValue parseTruthVal
    char '-'
    spaces
    body <- parseList parseTerm
    char '.'
    return $ Rule head body $ truthFetch controlStr "defaultTruthValue"

-- parseTruthVal :: (TruthClass a) => Parser a
-- parseTruthVal = do
--     spaces
--     invalid <- many ( letter )
--     spaces
--     return defaultTruthValue
    
parseRuleSimple :: String -> Parser Rule
parseRuleSimple controlStr = do
    head <- parseTerm
    spaces
    char '.'
    return $ Rule head [] $ truthFetch controlStr "defaultTruthValue"

parseRules :: String -> Parser Rules
parseRules controlStr = do
    spaces
    txt <- option "" $ parsePragma controlStr
    spaces
    first <- parseRule controlStr
    spaces
    next <- option mempty $ parseRules controlStr
    return $ first:next

parsePragma :: String -> Parser String
parsePragma oldControlStr = do
    spaces
    char '#'
    spaces
    lval <- many ( letter <|> digit )
    spaces
    char '='
    spaces
    rval <- many ( letter <|> digit )
    spaces
    if (lval == "controller")
       then return rval
       else return oldControlStr

parseText :: String -> Either ParseError Rules
-- parseText :: (TruthClass a) => String -> Either ParseError (Rules a)
parseText input = parse (parseRules "bool") "parseText" input

parseQuery :: Parser Term
parseQuery = do
    q <- parseTerm
    spaces
    optional $ char '.'
    return q
