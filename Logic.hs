module Logic
    ( TruthClass
    , conjunction, disjunction
    , defaultTruthValue, truthOfInference
    , unity, disunity
    , tcParse
    ) where


import Text.ParserCombinators.Parsec

class TruthClass a where

    -- these must be implemented
    conjunction :: a -> a -> a
    disjunction :: a -> a -> a
    
    unity :: a
    disunity :: a
    
    tcParse :: Parser a
    
    -- these defaults may be overridden to change operation
    defaultTruthValue :: a
    defaultTruthValue = unity
    
    truthOfInference :: a
    truthOfInference = unity

data TruthBox = forall a. TruthClass a => TruthBox a

data TruthController a = TruthController
    { tcUnity :: a 
    , tcDisunity :: a
    , tcConjunction :: a -> a -> a
    , tcDefaultTruthValue :: a
    , tcTruthOfInference :: a
    , pParse :: Parser a
    }
      
truthB :: TruthController Bool
truthB = TruthController
    { tcUnity = True
    , tcDisunity = False
    , tcConjunction = (&&)
    , tcDefaultTruthValue = True
    , tcTruthOfInference = True
    , pParse = do
        spaces
        word <- many ( letter <|> digit )
        spaces
        if ( word == "true" || word == "T" )
           then return True
           else return False
    }