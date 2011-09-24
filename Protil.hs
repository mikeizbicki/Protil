module Main where

import Interpreter

main = do
    rulesDB <- loadRules "examples/family.pl"
    repl rulesDB