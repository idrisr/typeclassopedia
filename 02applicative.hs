-- Maybe can easily be made an instance of Applicative
-- writing such an instance is left as an exercise for the reader

newtype MMaybe a = MMaybe (Maybe a)

instance Functor MMaybe where
    fmap f (MMaybe (Just a)) = MMaybe (Just $ f a)
    fmap f (MMaybe Nothing)  = MMaybe Nothing

instance Applicative MMaybe where
    pure a = MMaybe (Just a)
    _                 <*> (MMaybe Nothing)  = MMaybe Nothing
    (MMaybe Nothing)  <*> _                 = MMaybe Nothing
    (MMaybe (Just f)) <*> (MMaybe (Just a)) = MMaybe (Just $ f a)

newtype ZipList a = ZipList { getZipList :: [a] }

instance Functor ZipList where
    fmap f (ZipList []) = ZipList []
    fmap f (ZipList (x:xs)) = ZipList $ f x:fmap f xs

instance Applicative ZipList where
    pure a = ZipList [a]
    (ZipList gs) <*> (ZipList xs) = ZipList $ zipWith ($) gs xs
