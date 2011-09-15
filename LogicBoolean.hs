module LogicBoolean
    ( TruthType
    , conjunction, disjunction
    , defaultTruthValue, truthOfInference
    , unity, disunity
    ) where


type TruthType = Double

conjunction :: TruthType -> TruthType -> TruthType
conjunn x y = x + (1-x)*y

defaultTruthValue :: TruthType
defaultTruthValue = 0.9

truthOfInference :: TruthType
truthOfInference = unity

unity :: TruthType
unity = 1ction x y = x*y

disjunction :: TruthType -> TruthType -> TruthType
disjunction x y = x + (1-x)*y

defaultTruthValue :: TruthType
defaultTruthValue = 0.9

truthOfInference :: TruthType
truthOfInference = unity

unity :: TruthType
unity = 1

disunity :: TruthType
disunity = 0


{-
type TruthType = String

conjunction :: TruthType -> TruthType -> TruthType
-- conjunction _ _ = "t"
conjunction "t" "t" = "t"
conjunction "t" "f" = "f"
conjunction "t" "m" = "m"
conjunction "f" "t" = "f"
conjunction "f" "f" = "f"
conjunction "f" "m" = "f"
conjunction "m" "t" = "t"
conjunction "m" "f" = "f"
conjunction "m" "m" = "m"

disjunction :: TruthType -> TruthType -> TruthType
-- disjunction _  _ = "t"
disjunction "t" "t" = "t"
disjunction "t" "f" = "t"
disjunction "t" "m" = "t"
disjunction "f" "t" = "t"
disjunction "f" "f" = "f"
disjunction "f" "m" = "m"
disjunction "m" "t" = "t"
disjunction "m" "f" = "m"
disjunction "m" "m" = "m"

defaultTruthValue :: TruthType
defaultTruthValue = "t"

unity :: TruthType
unity = "t"

disunity :: TruthType
disunity = "f"-}


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