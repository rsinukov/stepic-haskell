newtype ZipList a = ZipList { getZipList :: [a] } deriving Show

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList $ map f xs

instance Applicative ZipList where
    pure x = ZipList $ repeat x
    (ZipList fs) <*> (ZipList xs) = ZipList $ zipWith ($) fs xs

(>$<) :: (a -> b) -> [a] -> [b]
(>$<) f xs = f <$> xs 

(>*<) :: [(a -> b)] -> [a] -> [b]
(>*<) f xs = getZipList $ (ZipList f) <*> (ZipList xs)

-- x1s = [1,2,3]
-- x2s = [4,5,6]
-- x3s = [7,8,9]
-- x4s = [10,11,12]

-- (\a b -> 2*a+3*b) >$< x1s >*< x2s
-- [14,19,24]

-- (\a b c -> 2*a+3*b+5*c) >$< x1s >*< x2s >*< x3s
-- [49,59,69]

-- (\a b c d -> 2*a+3*b+5*c-4*d) >$< x1s >*< x2s >*< x3s >*< x4s
-- [9,15,21]