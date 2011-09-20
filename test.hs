import Data.Functor
import Data.Monoid

data (Eq a) => SingleData a = SingleData a Int String -- etc.
data DataFail a = DataFail [SingleData a]
instance Functor DataFail where
    fmap f (DataFail xs) = DataFail (map f xs)
    
fmapWorks f (DataFail xs) = DataFail (map f xs)
    
instance Monoid (DataFail a) where
    mempty = DataFail []
    mappend (DataFail xs) (DataFail ys) = DataFail (xs ++ ys)

-- data DataTwo a b = DataTwo [SingleData a] b
-- instance Functor (DataTwo b) where
--     fmap f (DataTwo xs b) = DataTwo (map f xs) b

-- data DataTwo a b = DataTwo (SingleData a) [SingleData b]
-- instance Functor (DataTwo a) where
--     fmap f (DataTwo truth list) = DataTwo truth $ map f list

data DataSuccess1 a = DataSuccess1 [a]
instance Functor DataSuccess1 where
    fmap f (DataSuccess1 xs) = DataSuccess1 (map f xs)

-- data DataSuccess2 a = DataSuccess2 (SingleData a)
-- instance Functor DataSuccess2 where
--     fmap f (DataSuccess2 xs) = DataSuccess2 (f xs)

-- data (Eq a) => DataClass a = DataClass [a]
-- instance Functor DataClass where
--     fmap f (DataClass xs) = DataClass (map f xs)
--     
-- instance Monoid (DataClass a) where
--     mempty = DataClass []
--     mappend (DataClass xs) (DataClass ys) = DataClass (xs ++ ys)
