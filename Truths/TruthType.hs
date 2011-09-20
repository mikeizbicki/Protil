module TruthType 
    (
    ) where
        
data TruthType = TT_Boolean String 
               | TT_Maybe String 
               | TT_Percent Double
    deriving (Eq, Show)
