import Control.Applicative

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

instance Functor OddC where
    fmap f (Un a)       = Un $ f a
    fmap f (Bi x y odd) = Bi (f x) (f y) (fmap f odd)

instance Foldable OddC where
--  foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f ini (Un a)          = f a ini
    foldr f ini (Bi x y odd)    = f x (f y (foldr f ini odd))

instance Traversable OddC where
--  Applicative f => (a -> f b) -> OddC a -> f (OddC b)
    traverse f (Un a)           = pure Un <*> (f a)
    traverse f (Bi x y odd)     = pure Bi <*> (f x) <*> (f y) <*> (traverse f odd)

-- GHCi> cnt1 = Un 42
-- GHCi> cnt3 = Bi 1 2 cnt1
-- GHCi> cnt5 = Bi 3 4 cnt3
-- GHCi> cnt5
-- Bi 3 4 (Bi 1 2 (Un 42))
-- GHCi> cntInf = Bi 'A' 'B' cntInf
-- GHCi> cntInf
-- Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'B' (Bi 'A' 'Interrupted.

-- GHCi> (+1) <$> cnt5
-- Bi 4 5 (Bi 2 3 (Un 43))
-- GHCi> toList cnt5
-- [3,4,1,2,42]
-- GHCi> sum cnt5
-- 52
-- GHCi> traverse (\x->[x+2,x-2]) cnt1
-- [Un 44,Un 40]