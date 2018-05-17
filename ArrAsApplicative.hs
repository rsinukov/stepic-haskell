newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }

newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
    fmap f arr = Arr2 $ \x y -> f ((getArr2 arr) x y)
  
instance Functor (Arr3 e1 e2 e3) where
    fmap f arr = Arr3 $ \x y z -> f ((getArr3 arr) x y z)
  
instance Applicative (Arr2 e1 e2) where
    pure x = Arr2 $ \e1 e2 -> x
    -- (e1 -> e2 -> a -> b) -> (e1 -> e2 -> a) -> (e1 -> e2 -> b)
    (<*>) (Arr2 g) (Arr2 h) = Arr2 $ \e1 e2 -> g e1 e2 (h e1 e2)
  
instance Applicative (Arr3 e1 e2 e3) where
    pure x = Arr3 $ \e1 e2 e3 -> x
    -- (e1 -> e2 -> e3 -> a -> b) -> (e1 -> e2 -> e3 -> a) -> (e1 -> e2 -> e3 -> b)
    (<*>) (Arr3 g) (Arr3 h) = Arr3 $ \e1 e2 e3 -> g e1 e2 e3 (h e1 e2 e3)