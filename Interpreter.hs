module Interpreter 
    ( pr, loadRules
    ) where

import System.IO
import Control.OldException
-- import System.Console.Readline

import DataTypes
import Core
import Parser
import Logic

import Truths.Boolean

-- parsing interface

pr :: (TruthClass a, Eq a) => Rules a -> String -> [Bindings a]
pr rulesDB query = prove rulesDB [term]
    where term = right $ parse parseQuery "pr" query

loadRules :: (TruthClass a) => String -> IO (Rules a)
loadRules fileName = do
    handle <- openFile fileName ReadMode
    str <- hGetContents handle
    return $ right $ parseText str

-- test = unity

-- debug funcs

right :: Show a => Either a b -> b
right (Right r) = r
right (Left l) = error $ show l

rulesStr = unlines [
    "father(a,b).", 
    "father(b,c).",
    "father(c,d).",
    "ancestor(X,Y) :- father(X,Y).",
    "ancestor(X,Y) :- father(X,Z),ancestor(Z,Y).",
    "grandfather(X,Y) :- father(X,A), father(A,Y).",
    "foo(a,b,c,d,e,f).",
    "goop(X,a)."
    ]

-- rulesDBg = right $ parseText rulesStr

-- REPL loop

-- readEvalPrintLoop :: IO ()
-- readEvalPrintLoop = do
--     maybeLine <- readline "% "
--     case maybeLine of 
--          Nothing     -> return () -- EOF / control-d
--          Just "exit" -> return ()
--          Just line -> do addHistory line
--                         putStrLn $ "The user input: " ++ (show line)
--                         readEvalPrintLoop


-- repl :: Rules TruthBoolean -> IO () 
repl :: (TruthClass a, Show a, Eq a) => Rules a -> IO () 
repl rulesDB = do
    putStr "?- "
    nextLine <- getLine
--     try $ putStrLn $ show $ pr rulesDB nextLine
--     handle (\x -> putStrLn $ show x) (listPrint $ pr rulesDB nextLine)
    listPrint $ pr rulesDB nextLine
    repl rulesDB
    
listPrint :: Show a => [a] -> IO () -- FIXME: func doesn't fail on parse errors
listPrint [] = do return ()
listPrint (x:xs) = do
    putStrLn $ show x
    listPrint xs
    
main = do
    rulesDB <- if True 
                  then loadRules "examples/family.pl" :: IO (Rules Boolean)
                  else loadRules "examples/family.pl" :: IO (Rules Double)
    repl rulesDB
--     readEvalPrintLoop