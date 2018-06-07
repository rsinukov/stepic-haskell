import Data.Char
import Control.Applicative hiding (many)

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
    fmap f p = Prs fun where
        fun s = (\(a, s') -> (f a, s')) <$> runPrs p s

anyChr :: Prs Char
anyChr = Prs fun where
    fun "" = Nothing
    fun (c:xs) = Just (c, xs)

char :: Char -> Prs Char
char c = Prs fun where
    fun [] = Nothing
    fun (x:xs) | c == x = Just (c, xs)
               | otherwise = Nothing

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