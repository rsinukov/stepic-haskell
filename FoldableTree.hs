import Data.Monoid

data Triple a = Tr a a a  deriving (Eq,Show)


data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

tree = Branch (
    Branch 
        (Branch Nil 31 Nil) 
        21 
        (Branch Nil 32 Nil)) 
    1 
    (Branch 
        (Branch Nil 33 Nil) 
        22 
        (Branch Nil 34 Nil)
    )

newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

-- foldr :: (a -> b -> b) -> b -> t a -> b
-- foldMap :: Monoid m => (a -> m) -> t a -> m

instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch left x right) = l . head . r $ ini where
        head = \i -> f x i
        l = \i -> foldr f i left
        r = \i -> foldr f i right

    foldMap f m = foldr (mappend . f) mempty m

instance Foldable Preorder where
    foldr f ini (PreO Nil) = ini
    foldr f ini (PreO (Branch left x right)) = head . l . r $ ini where
        head = \i -> f x i
        l = \i -> foldr f i (PreO left)
        r = \i -> foldr f i (PreO right)
    
    foldMap f m = foldr (mappend . f) mempty m
    
instance Foldable Postorder where
    foldr f ini (PostO Nil) = ini
    foldr f ini (PostO (Branch left x right)) = l . r . head $ ini where
        head = \i -> f x i
        l = \i -> foldr f i (PostO left)
        r = \i -> foldr f i (PostO right)

    foldMap f m = foldr (mappend . f) mempty m

instance Foldable Levelorder where
    foldr f ini (LevelO Nil)                    = ini
    foldr f ini (LevelO (Branch left x right))  = f x (helper [LevelO left, LevelO right]) where
        helper [] = ini
        helper ((LevelO Nil) : xs) = helper xs
        helper ((LevelO (Branch left x right)) : xs) = f x (helper $ xs ++ [LevelO left, LevelO right])
    
    foldMap f m = foldr (mappend . f) mempty m
    