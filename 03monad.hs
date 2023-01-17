import Prelude hiding (fmap, return )
-- the trivial monad
-- http://blog.sigfpe.com/2007/04/trivial-monad.html
--

newtype W a = W a deriving Show

-- wrap data
return :: a -> W a
return = W

-- manipulate data and leave it wrapped
fmap :: (a->b) -> W a -> W b
fmap f (W x) = W $ f x

bind :: (a -> W b) -> W a -> W b
bind f (W x) = f x

dinb:: W a -> (a -> W b) -> W b
dinb = flip bind

f :: Int -> W Int
f x = W $ x + 1

c = bind f $ f 1
d = bind f $ bind f $ f 1

-- (1) define a function
-- g :: Int -> W Int -> W Int
-- so that g x (W y) = W (x+y). Obviously
-- that definition won't do -
-- the left hand side has a W y pattern so it's actually unwrapping.
-- Rewrite this function so that the only unwrapping that happens is carried out by bind.
g :: Int -> W Int -> W Int
-- g x = bind (\z -> W (z+x))
g x = bind (return . (+x))
-- g x (W y) = W (x+y)

a = W 3
b = W 2

-- (2) define a function
h :: W Int -> W Int -> W Int
h x = bind (\a -> bind (\b -> W(a+b)) x)
-- h (W x) (W y) = W (x+y).
-- Again, no unwrapping.
