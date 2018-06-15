{-# LANGUAGE TypeOperators #-}

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) } 
    deriving (Eq, Show)

type A   = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C   = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps (1, ('a', True))

b :: B t
b = Cmps (True, id, Right 1)

c :: C
c  = Cmps (\b a -> if b then a else (a + 1))



newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) } 
  deriving (Eq,Show) 

instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
  fmap q (Cmps3 x) = Cmps3 $ fmap (fmap (fmap q)) x