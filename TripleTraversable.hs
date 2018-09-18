import Control.Applicative
import Data.Monoid

data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
    fmap f (Tr arg1 agr2 agr3) = Tr (f arg1) (f agr2) (f agr3)

instance Applicative Triple where
    pure x = Tr x x x
    (Tr f1 f2 f3) <*> (Tr arg1 agr2 agr3) = Tr (f1 arg1) (f2 agr2) (f3 agr3) 

instance Foldable Triple where
--  foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f ini (Tr a b c) = (f a . f b . f c) ini
--  foldMap :: Monoid m => (a -> m) -> t a -> m
    foldMap f (Tr a b c) =  f a <> f b <> f c

instance Traversable Triple where
--  traverse :: Applicative f => (a -> f b) -> Triple a -> f (Triple b)
    traverse f (Tr a b c) = pure Tr <*> (f a) <*> (f b) <*> (f c)

-- GHCi> foldl (++) "!!" (Tr "ab" "cd" "efg")
-- "!!abcdefg"
-- GHCi> traverse (\x -> if x>10 then Right x else Left x) (Tr 12 14 16)
-- Right (Tr 12 14 16)
-- GHCi> traverse (\x -> if x>10 then Right x else Left x) (Tr 12 8 4)
-- Left 8
-- GHCi> sequenceA (Tr (Tr 1 2 3) (Tr 4 5 6) (Tr 7 8 9))
-- Tr (Tr 1 4 7) (Tr 2 5 8) (Tr 3 6 9)