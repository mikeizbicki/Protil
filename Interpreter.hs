module Interpreter 
    ( repl, pr, loadRules
    ) where

import System.IO
import System.Console.Haskeline
import Text.ParserCombinators.Parsec

import DataTypes
import Core
import Parser
import Logic

---------------------------------------
-- REPL

repl :: RulesDB -> IO ()
repl rulesDB = runInputT defaultSettings loop
    where 
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "?- "
            case minput of
                Nothing -> return ()
                Just input -> do prettyPrint $ pr rulesDB input
                                 loop


---------------------------------------
-- Parsing interface

pr :: RulesDB -> String -> Either ParseError [TruthList]
pr rulesDB query = case eterm of 
                        Left err -> Left err
                        Right term -> Right $ prove rulesDB [term]
    where eterm = parse parseQuery "pr" query

loadRules :: String -> IO RulesDB
loadRules fileName = do
    handle <- openFile fileName ReadMode
    str <- hGetContents handle
    return $ right $ parseText str

-- debug funcs

right :: Show a => Either a b -> b
right (Right r) = r
right (Left l) = error $ "supid me" ++ show l

---------------------------------------
-- Pretty print
    
prettyPrint :: Either ParseError [TruthList] -> InputT IO ()
prettyPrint (Left e        ) = outputStrLn $ show e
prettyPrint (Right []    ) = do return ()
prettyPrint (Right (b:bs)) = do
    outputStrLn $ ppShowBindings b
    prettyPrint $ Right bs

ppShowBindings :: TruthList -> String
ppShowBindings x = ppShowBindings' (show $ getTruth x) (getTermPairs x) (getRules x)

ppShowBindings' :: String -> [Binding] -> [Rule] -> String
ppShowBindings' truthStr []     rs = ""
ppShowBindings' truthStr (b:bs) rs = truthStr ++ ": " ++ ppShowBindingList (b:bs) ++ tracerStr
    where tracerStr = if length rs > 0
                          then "         :tracer: " ++ show rs
                          else ""
                                  
ppShowBindingList :: [Binding] -> String
ppShowBindingList xs = "[" ++ ppShowBindingList' xs ++ "]"
    where ppShowBindingList' [x]    = ppShowBinding x
          ppShowBindingList' (x:xs) = ppShowBinding x ++ "," ++ ppShowBindingList' xs
                                  
ppShowBinding :: Binding -> String
ppShowBinding (x,y) = show x ++ "=" ++ show y

getTruth :: TruthList -> TruthBox
getTruth (TruthList (TracerBox tb _) _) = tb

getRules :: TruthList -> [Rule]
getRules (TruthList (TracerBox _ rs) _) = rs

getTermPairs :: TruthList -> [Binding]
getTermPairs (TruthList _ p) = p
