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
parseRule controlStr = do
    head <- parseTerm
    spaces
    body <- option [] parseBody
    spaces
    truthVal <-option (truthFetch controlStr "defaultTruthValue") parseTruthVal
    spaces
    char '.'
    return $ Rule head body truthVal
        where parseBody = do
                  char ':'
                  char '-'
                  spaces
                  parseList parseTerm
              parseTruthVal = do
                  char '<'
                  char '-'
                  spaces
                  parseTruthBox

parseRules :: String -> Parser RulesDB
parseRules controlStr = do
    spaces
    controlStrNew <- option controlStr $ parsePragma
    spaces
    first <- parseRule controlStrNew
    spaces
    RulesDB oldController next <- option (RulesDB "" []) $ parseRules controlStrNew
    return $ RulesDB controlStrNew (first:next)

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
    return rval

parseText :: String -> Either ParseError RulesDB
parseText input = parse (parseRules defaultTruth) "parseText" input

parseQuery :: Parser Term
parseQuery = do
    q <- parseTerm
    spaces
    optional $ char '.'
    return q
