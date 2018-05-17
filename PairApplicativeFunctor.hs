-- instance Monoid e => Applicative ((,) e) where
--    pure x = (mempty, x)
--    (a, f) <*> (b, x) = (mappend a b, f x) 

divideList :: Fractional a => [a] -> a
divideList []     = 1
divideList (x:xs) = (/) x (divideList xs)

divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = ("1.0", 1)
divideList' (x:xs) = (/) <$> (log x, x) <*> divideList' xs where 
    log x = "<-" ++ (show x) ++ "/"