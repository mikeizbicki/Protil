module LogicBoolean
    ( 

    
    ) where

-- generic definitions




-- unity :: TruthType
-- disunity :: TruthType
-- 
-- defaultTruthValue :: String -> TruthType
-- defaultTruthValue _ = unity
-- 
-- truthOfInference :: TruthType
-- truthOfInference = unity

-- TT_Percent

-- unity = TT_Percent 1
-- disunity = TT_Percent 0

-- TT_Maybe


-- conjunction (TT_Maybe "t") (TT_Maybe "t") = TT_Maybe "t"
-- conjunction (TT_Maybe "t") (TT_Maybe "f") = TT_Maybe "f"
-- conjunction (TT_Maybe "t") (TT_Maybe "m") = TT_Maybe "m"
-- conjunction (TT_Maybe "f") (TT_Maybe "t") = TT_Maybe "f"
-- conjunction (TT_Maybe "f") (TT_Maybe "f") = TT_Maybe "f"
-- conjunction (TT_Maybe "f") (TT_Maybe "m") = TT_Maybe "f"
-- conjunction (TT_Maybe "m") (TT_Maybe "t") = TT_Maybe "t"
-- conjunction (TT_Maybe "m") (TT_Maybe "f") = TT_Maybe "f"
-- conjunction (TT_Maybe "m") (TT_Maybe "m") = TT_Maybe "m"
-- 
-- disjunction (TT_Maybe "t") (TT_Maybe "t") = TT_Maybe "t"
-- disjunction (TT_Maybe "t") (TT_Maybe "f") = TT_Maybe "t"
-- disjunction (TT_Maybe "t") (TT_Maybe "m") = TT_Maybe "t"
-- disjunction (TT_Maybe "f") (TT_Maybe "t") = TT_Maybe "t"
-- disjunction (TT_Maybe "f") (TT_Maybe "f") = TT_Maybe "f"
-- disjunction (TT_Maybe "f") (TT_Maybe "m") = TT_Maybe "m"
-- disjunction (TT_Maybe "m") (TT_Maybe "t") = TT_Maybe "t"
-- disjunction (TT_Maybe "m") (TT_Maybe "f") = TT_Maybe "m"
-- disjunction (TT_Maybe "m") (TT_Maybe "m") = TT_Maybe "m"
-- 
-- unity = TT_Maybe "t"
-- disunity = TT_Maybe "f"


{-

type TruthType = String

conjunction :: TruthType -> TruthType -> TruthType
-- conjunction _ _ = "t"
conjunction "t" "t" = "t"
conjunction "t" "f" = "f"
conjunction "f" "t" = "f"
conjunction "f" "f" = "f"

disjunction :: TruthType -> TruthType -> TruthType
-- disjunction _  _ = "t"
disjunction "t" "t" = "t"
disjunction "t" "f" = "t"
disjunction "f" "t" = "t"
disjunction "f" "f" = "f"

defaultTruthValue :: TruthType
defaultTruthValue = unity

unity :: TruthType
unity = "t"

disunity :: TruthType
disunity = "f"
-}