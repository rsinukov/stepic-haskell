traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list func = foldr (\x y -> (:) <$> func x <*> y) (pure [])
