newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}
newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

instance Functor (Arr2 e1 e2) where
  fmap f arr = Arr2 $ \x y -> f ((getArr2 arr) x y)

instance Functor (Arr3 e1 e2 e3) where
  fmap f arr = Arr3 $ \x y z -> f ((getArr3 arr) x y z)