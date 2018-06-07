import Data.Char

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
    fmap f p = Prs fun where
        fun s = (\(a, s') -> (f a, s')) <$> runPrs p s

anyChr :: Prs Char
anyChr = Prs fun where
    fun "" = Nothing
    fun (c:xs) = Just (c, xs)

-- GHCi> runPrs anyChr "ABC"
-- Just ('A',"BC")
-- GHCi> runPrs anyChr ""
-- Nothing
-- GHCi> runPrs (digitToInt <$> anyChr) "BCD"
-- Just (11,"CD")