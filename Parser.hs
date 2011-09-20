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
import Truths.Boolean

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
    
parseRule :: (TruthClass a) => Parser (Rule a)
parseRule = try parseRuleComplex <|> parseRuleSimple
    
parseRuleComplex :: (TruthClass a) => Parser (Rule a)
parseRuleComplex = do
    head <- parseTerm
    spaces
    char ':'
    truthVal <- option defaultTruthValue parseTruthVal
    char '-'
    spaces
    body <- parseList parseTerm
    char '.'
    return $ Rule head body truthVal--defaultTruthValue

parseTruthVal :: (TruthClass a) => Parser a
parseTruthVal = do
    spaces
    invalid <- many ( letter )
    spaces
    return defaultTruthValue
    
parseRuleSimple :: (TruthClass a) => Parser (Rule a)
parseRuleSimple = do
    head <- parseTerm
    spaces
    char '.'
    return $ Rule head [] defaultTruthValue

parseRules :: (TruthClass a) => Parser (Rules a)
parseRules = do
    spaces
    txt <- option "" parsePragma
    spaces
    first <- parseRule
    spaces
    next <- option mempty parseRules
    return $ (Rules [first]) `mappend` next

parsePragma :: Parser String
parsePragma = do
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
       else return ""

parseText :: (TruthClass a) => String -> Either ParseError (Rules a)
-- parseText :: (TruthClass a) => String -> Either ParseError (Rules a)
parseText input = parse parseRules "parseText" input

parseQuery :: Parser Term
parseQuery = do
    q <- parseTerm
    spaces
    optional $ char '.'
    return q
