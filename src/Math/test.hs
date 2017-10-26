{-# LANGUAGE TypeFamilies,MultiParamTypeClasses, FlexibleInstances #-} --FlexibleInstances, UndecidableInstances, AllowAmbiguousTypes #-}

class Collection f where
  type Element f :: *
  insert :: Element f -> f
  add_ :: f -> f -> f

instance Collection [a] where
  type Element [a] = a
  insert x = [x]
  add_ xs ys = xs ++ ys

class Concat a b f where
  con :: a -> b -> f

instance (Collection f, a ~ Element f) => Concat a f f where
  con = con10

instance (Collection f, a ~ Element f) => Concat f a f where
  con = con01

test10 :: [String]
test10 = con "a" ["b","c"]

test01 :: [String]
test01 = con ["b","c"] "a"

-- instance (Concat' (Select a b) a b f) => Concat a b f where
--   con = con' (undefined::Select a b)

class Concat' t a b f where
  con' :: t -> a -> b -> f

data OneOne
data OneZero
data ZeroOne
data ZeroZero

type family Select a b where
  Select a a = OneOne
--   Select a f f = OneZero
--   Select f a f = ZeroOne
--   --Select f f f = ZeroZero

instance (Collection f, a ~ Element f, b ~ f) => Concat' OneZero a b f where
  con' _ x ys = add_ (insert x) ys

instance (Collection f, a ~ f, b ~ Element f) => Concat' ZeroOne a b f where
  con' _ xs y = add_ xs (insert y)

instance (Collection f, a ~ f, b ~ f) => Concat' ZeroZero a b f where
  con' _ xs ys = add_ xs ys

instance (Collection f, a ~ Element f, b ~ Element f) => Concat' OneOne a b f where
  con' _ x y = add_ (insert x) (insert y)

con10 :: (Collection f, a ~ Element f) => a -> f -> f
con10 = con' (undefined::OneZero)

con01 :: (Collection f, a ~ Element f) => f -> a -> f
con01 = con' (undefined::ZeroOne)

con00 :: (Collection f) => f -> f -> f
con00 = con' (undefined::ZeroZero)

con11 :: (Collection f, a~Element f, b~Element f) => a -> b -> f
con11 = con' (undefined::OneOne)
