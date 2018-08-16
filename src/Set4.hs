{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude
import Set2

newtype Gen a = Gen { run:: Seed -> (a, Seed) }

class Monad m where
  return :: a -> m a
  bind :: m a -> (a -> m b) -> m b
  (>>=) :: m a -> (a -> m b) -> m b
  (>>=) = bind
  map :: m a -> (a -> b) -> m b
  map ma f = bind ma (\a -> return $ f a)
  liftM2 :: (a -> b -> c) -> m a -> m b -> m c
  liftM2 f ma mb = (bind ma (\a -> bind mb (\b -> return $ f a b)))
  sequence :: [m a] -> m [a]
  sequence [] = return []
  sequence (ma:mas) = liftM2 (:) ma (Set4.sequence mas)
  (=<<) :: (a -> m b) -> m a -> m b
  (=<<) = flip bind
  join :: m (m a) -> m a
  join m = bind m id
  ap :: m (a -> b) -> m a -> m b
  ap mf ma = liftM2 (\f -> \a -> f a) mf ma

instance Monad [] where 
  return a = [a]
  bind [] _ = []
  bind (x:xs) f = f x ++ bind xs f

instance Monad Maybe where
  return = Just
  bind Nothing _ = Nothing
  bind (Just x) f = f x

instance Monad Gen where 
  return a = Gen $ \s -> (a, s)
  bind (Gen run) f = Gen $ \s -> 
    let (a,s2) = run s
        Gen run2 = f a
    in run2 s2

evalGen :: Gen a -> Seed -> a
evalGen (Gen run) s = fst $ run s