import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

data State s a = State { runState :: (s -> (a, s)) }

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  m >>= k  = State $ \st -> let 
    (x1, st1) = runState m st
    m1 = k x1 
    in runState m1 st1
     

instance Functor (State r) where
  fmap = liftM

instance Applicative (State r) where
  pure  = return
  (<*>) = ap

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

--

data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving Show

numberTree :: Tree () -> Tree Integer
numberTree tree = fst $ runState (countTree tree) 1

countTree :: Tree () -> State Integer (Tree Integer)
countTree (Leaf ())               = State $ \n -> (Leaf n, n + 1)
countTree (Fork left () right)    = do
  leftCounted <- countTree left
  leftCount <- get
  put (leftCount + 1)
  rightCounted <- countTree right
  State $ \n -> (Fork leftCounted leftCount rightCounted, n)