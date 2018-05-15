import Control.Applicative

data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
    fmap f (Tr arg1 agr2 agr3) = Tr (f arg1) (f agr2) (f agr3)

instance Applicative Triple where
    pure x = Tr x x x
    (Tr f1 f2 f3) <*> (Tr arg1 agr2 agr3) = Tr (f1 arg1) (f2 agr2) (f3 agr3) 

-- (^2) <$> Tr 1 (-2) 3
-- Tr 1 4 9

-- Tr (^2) (+2) (*3) <*> Tr 2 3 4
-- Tr 4 5 12