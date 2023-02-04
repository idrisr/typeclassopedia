-- 1 Implement Functor instances for Either e and ((->) e).
newtype Eether a b = Eether (Either a b)
instance Functor (Eether a) where
    fmap f (Eether (Right b)) = Eether $ Right $ f b
    fmap _ (Eether (Left a))  = Eether $ Left a

newtype Func a b = Func (a -> b)
instance Functor (Func a) where
    -- f :: a->b
    -- g :: e->a
    fmap f (Func g) = Func $ f . g

-- 2 Implement Functor instances for ((,) e) and for Pair, defined as
-- Explain their similarities and differences.
-- Pair takes one type parameter
-- Tople takes two type parameters
data Pair a = Pair a a
instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) $ f b

newtype Tople a b = Tople (a, b)
instance Functor (Tople a) where
    fmap f (Tople (x, y)) = Tople (x, f y)

-- 3. Implement a Functor instance for the type ITree, defined as
data ITree a = Leaf (Int -> a)
             | Node [ITree a]

instance Functor ITree where
    fmap f (Leaf g)  = Leaf $ f . g
    fmap f (Node xs) = Node $ fmap g xs
        where g = fmap f

-- 4. Give an example of a type of kind * -> * which cannot be made an instance
-- of Functor (without using undefined).
newtype Funk a b = Funk (b -> a)

-- instance Functor (Funk z) where
--  fmap f (Funk g)
    -- f :: a -> b
    -- g :: z -> b
    -- f and g cant be composed for fmap

-- 5. Is this statement true or false?
-- The composition of two Functors is also a Functor.
-- If false, give a counterexample; if true, prove it by exhibiting some
-- appropriate Haskell code.
--
-- f a
-- G a
