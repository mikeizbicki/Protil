class (Read a, Show a, Eq a) => TruthClass a where
    conjunction :: a -> a -> a
    disjunction :: a -> a -> a
    classReturn :: String -> a
    classReturn = read

instance TruthClass Bool where
    conjunction = (&&)
    disjunction = (||)

instance TruthClass Double where
    conjunction x y = x*y
    disjunction x y = x + (1-x)*y