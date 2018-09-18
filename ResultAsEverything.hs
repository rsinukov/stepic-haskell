import Control.Applicative
import Data.Monoid

data Result a = Ok a | Error String deriving (Eq,Show)

instance Functor Result where
    fmap f (Ok a)       = Ok $ f a
    fmap _ (Error a)    = (Error a)

instance Applicative Result where
    pure a                  = Ok a
    (Ok func) <*> (Ok arg)  = Ok (func arg)
    (Error a) <*> _         = Error a
    _ <*> Error a           = Error a

instance Foldable Result where
--  foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f ini (Ok a)      = f a ini
    foldr f ini (Error a)   = ini

instance Traversable Result where
--  Applicative f => (a -> f b) -> Result a -> f (Result b)
    traverse f (Ok a)       = pure Ok <*> f a
    traverse _ (Error a)    = pure $ Error a

-- GHCi> traverse (\x->[x+2,x-2]) (Ok 5)
-- [Ok 7,Ok 3]
-- GHCi> traverse (\x->[x+2,x-2]) (Error "!!!")
-- [Error "!!!"]