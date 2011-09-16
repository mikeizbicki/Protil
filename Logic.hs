module Logic
    ( TruthClass
    , conjunction, disjunction
    , defaultTruthValue, truthOfInference
    , unity, disunity
    ) where

class TruthClass a where

    -- these must be implemented
    conjunction :: a -> a -> a
    disjunction :: a -> a -> a
    
    unity :: a
    disunity :: a
    
    -- these defaults may be overridden to change operation
    defaultTruthValue :: a
    defaultTruthValue = unity
    
    truthOfInference :: a
    truthOfInference = unity
