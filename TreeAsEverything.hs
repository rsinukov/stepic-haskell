import Control.Applicative
import Data.Monoid

data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

instance Functor Tree where
    fmap _ Nil                      = Nil
    fmap f (Branch left a right)    = Branch (fmap f left) (f a) (fmap f right)

instance Applicative Tree where
    pure a                  = Branch (pure a) a (pure a)
    Nil <*> tree            = Nil
    func <*> Nil            = Nil
    (Branch leftF f rightF) <*> (Branch left a right) = Branch (leftF <*> left) (f a) (rightF <*> right)

instance Foldable Tree where
--  foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f ini Nil                     = ini
    foldr f ini (Branch left a right)   = leftF . nodeF . rightF $ ini where
        nodeF   = \i -> f a i
        leftF   = \i -> foldr f i left
        rightF  = \i -> foldr f i right

instance Traversable Tree where
--  Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse f Nil                      = pure Nil
    traverse f (Branch left a right)    = pure Branch <*> (traverse f left) <*> (f a) <*> (traverse f right)

-- GHCi> traverse (\x->[x+2,x-2]) (Ok 5)
-- [Ok 7,Ok 3]
-- GHCi> traverse (\x->[x+2,x-2]) (Error "!!!")
-- [Error "!!!"]