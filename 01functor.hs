
newtype Tuple a b = Tuple (a, b)
newtype Func a b = Func ((->) a b)
newtype EZ a b = EZ (Either a b)

instance Functor (Tuple a) where
    fmap f (Tuple (a, b)) = Tuple (a, f b)

instance Functor (Func a) where
    fmap g (Func f) = Func (g . f)

instance Functor (EZ a) where
    fmap f (EZ (Left a)) = EZ (Left a)
    fmap f (EZ (Right b)) = EZ (Right (f b))

-- A good exercise is to implement Functor instances for
-- Either e
-- ((,) e)
-- ((->e)f
