module Parser
    ( parseText
    , parseQuery
    , parse
    ) where

import Data.Char
import Text.ParserCombinators.Parsec

import DataTypes
import LogicBoolean

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
    
parseRule :: TruthController -> Parser Rule
parseRule truthController = try (parseRuleComplex truthController) <|> (parseRuleSimple truthController)
    
parseRuleComplex :: TruthController -> Parser Rule
parseRuleComplex truthController = do
    head <- parseTerm
    spaces
    string ":-"
    spaces
    body <- parseList parseTerm
    char '.'
    return $ Rule head body $ defaultTruthValue truthController

parseRuleSimple :: TruthController -> Parser Rule
parseRuleSimple truthController = do
    head <- parseTerm
    spaces
    char '.'
    return $ Rule head [] $ defaultTruthValue truthController

parseRules :: TruthController -> Parser Rules
parseRules truthController = do
    spaces
    first <- parseRule truthController
    spaces
    next <- option [] $ parseRules truthController
--     next <- option [] $ do
--         next <- parseRules truthController
--         return next
    return $ ruleCons first next
    
parseTruthMode :: Parser String
parseTruthMode = do
    spaces
    char '#'
    spaces
    spaces
    return "test"
    
parseText :: String -> Either ParseError Rules
parseText input = parse (parseRules prolog) "parseText" input

parseQuery :: Parser Term
parseQuery = do
    q <- parseTerm
    spaces
    optional $ char '.'
    return q
