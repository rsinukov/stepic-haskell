import Control.Applicative

newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP pr = PrsEP fun where
    fun pos []      = (pos + 1, Left $ "pos " ++ (show (pos + 1)) ++ ": unexpected end of input")
    fun pos (c:cx)  | pr c      = (pos + 1, Right (c, cx))  
                    | otherwise = (pos + 1, Left $ "pos " ++ (show (pos + 1)) ++ ": unexpected " ++ [c])
                        
charEP :: Char -> PrsEP Char
charEP c = satisfyEP (== c)

anyEP :: PrsEP Char
anyEP = satisfyEP (const True)

instance Functor PrsEP where
    fmap f p = PrsEP fun where
        fun pos s = (\either -> (\(x, s') -> (f x, s')) <$> either) <$> (runPrsEP p pos s)
  
instance Applicative PrsEP where
    pure a = PrsEP $ \pos s -> (pos, Right (a, s))
    pf <*> pv = PrsEP fun where
        fun pos s = case (runPrsEP pf pos s) of
            (pos1, (Left error)) -> (pos1, Left error)
            (pos1, (Right (f, s'))) -> runPrsEP (f <$> pv) pos1 s'

instance Alternative PrsEP where
    -- empty :: f a
    empty = PrsEP $ \pos s -> (pos, Left $ "pos " ++ (show pos) ++ ": empty alternative")
    -- (<|>) :: f a -> f a -> f a
    p <|> q = PrsEP fun where
        fun pos s = select (runPrsEP p pos s) (runPrsEP q pos s) where
            select (pos1, val1@(Right a)) _ = (pos1, val1)
            select (pos1, val1@(Left a)) (pos2, val2@(Right _)) = (pos2, val2)
            select (pos1, val1) (pos2, val2) = if (pos1 >= pos2) then (pos1, val1) else (pos2, val2)

-- GHCi> runPrsEP empty 0 "ABCDEFG"
-- (0,Left "pos 0: empty alternative")
-- GHCi> charEP c = satisfyEP (== c)
-- GHCi> tripleP [a,b,c] = (\x y z -> [x,y,z]) <$> charEP a <*> charEP b <*>  charEP c
-- GHCi> parseEP (tripleP "ABC" <|> tripleP "ADC") "ABE"
-- Left "pos 3: unexpected E"
-- GHCi> parseEP (tripleP "ABC" <|> tripleP "ADC") "ADE"
-- Left "pos 3: unexpected E"
-- GHCi> parseEP (tripleP "ABC" <|> tripleP "ADC") "AEF"
-- Left "pos 2: unexpected E"