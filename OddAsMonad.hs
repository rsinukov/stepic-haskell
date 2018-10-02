import Control.Monad

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

instance Functor OddC where
    fmap f (Un a)       = Un $ f a
    fmap f (Bi x y odd) = Bi (f x) (f y) (fmap f odd)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a) (Un b) z           = Bi a b z
concat3OC (Un a) (Bi b c next) y    = Bi a b (concat3OC (Un c) next y)
concat3OC (Bi a b next) x y         = Bi a b (concat3OC next x y)

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un a) = a
concatOC (Bi a b c) = concat3OC a b (concatOC c)

instance Applicative OddC where
    pure = return
    (<*>) (Un f) sx             = fmap f sx
    (<*>) (Bi af bf offF) sx    = concat3OC (fmap af sx) (fmap bf sx) (offF <*> sx)

instance Monad OddC where
    return a = Un a
--  (>>=) :: Odd a -> (a -> Odd b) -> Odd b  
    (>>=) xs f = concatOC $ fmap f xs
