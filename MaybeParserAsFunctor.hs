import Data.Char
import Control.Applicative hiding (many)

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
    fmap f p = Prs fun where
        fun s = (\(a, s') -> (f a, s')) <$> runPrs p s

instance Applicative Prs where
    pure a = Prs $ \s -> Just (a, s)
    pf <*> pv = Prs fun where 
        fun s = do
            (f, s') <- runPrs pf s
            (v, s'') <- runPrs pv s'
            return (f v, s'')

-- GHCi> runPrs anyChr "ABC"
-- Just ('A',"BC")
-- GHCi> runPrs anyChr ""
-- Nothing
-- GHCi> runPrs (digitToInt <$> anyChr) "BCD"
-- Just (11,"CD")

instance Alternative Prs where
    -- empty :: f a
    empty = Prs $ \s -> Nothing
    -- (<|>) :: f a -> f a -> f a
    p <|> q = Prs fun where
       fun s = (runPrs p s) <|> (runPrs q s)

-- GHCi> runPrs (char 'A' <|> char 'B') "ABC"
-- Just ('A',"BC")
-- GHCi> runPrs (char 'A' <|> char 'B') "BCD"
-- Just ('B',"CD")
-- GHCi> runPrs (char 'A' <|> char 'B') "CDE"
-- Nothing

many :: Prs a -> Prs [a]
many prs = (:) <$> prs <*> many prs <|> pure []

many1 :: Prs a -> Prs [a]
many1 prs = (:) <$> prs <*> many prs

-- > runPrs (many1 $ char 'A') "AAABCDE"
-- Just ("AAA","BCDE")
-- > runPrs (many1 $ char 'A') "BCDE"
-- Nothing

digitCount :: Int -> Int
digitCount x = helper 0 x where 
    helper count 0 = count
    helper count x = helper (count + 1) (x `div` 10)

satisfy :: (Char -> Bool) -> Prs Char
satisfy pr = Prs fun where
    fun ""      = Nothing
    fun (c:cx)  | pr c      = Just (c, cx) 
                | otherwise = Nothing

anyChr :: Prs Char
anyChr = satisfy $ const True

char :: Char -> Prs Char
char c = satisfy (\x -> c == x)
    
digit :: Prs Char
digit = satisfy isDigit

nat :: Prs Int
nat = read <$> many1 digit 

dbl :: Prs Double
dbl = double <|> natAsDbl where
    double = (+) <$> natAsDbl <* char '.' <*> fractional
    natAsDbl = (fromIntegral <$> nat)
    fractional = (\x -> (fromIntegral x) / (10 ^ (fromIntegral $ digitCount x))) <$> nat

mult :: Prs Double
mult = (*) <$> dbl <* char '*' <*> dbl

-- GHCi> runPrs mult "2.2*10"
-- Just (22.0,"")
-- GHCi> runPrs mult "2*10.1"
-- Just (20.2,"")
-- GHCi> runPrs mult "2.9*3.9aa"
-- Just (11.309999999999999,"aa")