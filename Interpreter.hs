module Interpreter 
    ( repl, pr, loadRules
    ) where

import System.IO
import System.Console.Haskeline

import DataTypes
import Core
import Parser
import Logic

-- parsing interface

pr :: RulesDB -> String -> [Bindings]
pr rulesDB query = prove rulesDB [term]
    where term = right $ parse parseQuery "pr" query

loadRules :: String -> IO RulesDB
loadRules fileName = do
    handle <- openFile fileName ReadMode
    str <- hGetContents handle
    return $ right $ parseText str

-- debug funcs

right :: Show a => Either a b -> b
right (Right r) = r
right (Left l) = error $ show l

-- REPL loop
    
repl :: RulesDB -> IO ()
repl rulesDB = runInputT defaultSettings loop
    where 
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "% "
            case minput of
                Nothing -> return ()
                Just input -> do evalPrint rulesDB input
                                 loop
                                 
evalPrint rulesDB input = listPrint $ pr rulesDB input --catch (listPrint $ pr rulesDB input) (putStrLn ("Caught " ++ show (e :: IOException))
                                 
listPrint :: Show a => [a] -> InputT IO ()
listPrint [] = do return ()
listPrint (x:xs) = do
    outputStrLn $ show x
    listPrint xs
    
---------------------------------------
-- Pretty print

ppShowBinding :: Binding -> String
ppShowBinding (Var v, Atom a) = v ++ "=" ++ a

ppShowBindings :: Bindings -> String
ppShowBindings x = show x

    