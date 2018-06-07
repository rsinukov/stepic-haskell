newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE pr = PrsE fun where
    fun ""      = Left "unexpected end of input"
    fun (c:cx)  | pr c      = Right (c, cx) 
                | otherwise = Left $ "unexpected " ++ [c]

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

instance Functor PrsE where
    fmap f p = PrsE fun where
        fun s = (\(x, s') -> (f x, s')) <$> runPrsE p s
  
instance Applicative PrsE where
    pure a = PrsE $ \s -> Right (a, s)
    (<*>) pf pv = PrsE fun where 
        fun s = do
            (f, s') <- runPrsE pf s
            (v, s'') <- runPrsE pv s'
            return (f v, s'')

-- GHCi> let anyE = satisfyE (const True)
-- GHCi> runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "ABCDE"
-- Right (('A','C'),"DE")
-- GHCi> runPrsE ((,) <$> anyE <* charE 'C' <*> anyE) "ABCDE"
-- Left "unexpected B"
-- GHCi> runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "AB"
-- Left "unexpected end of input"