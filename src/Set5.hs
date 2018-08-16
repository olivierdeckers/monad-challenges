{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set5 where

import MCPrelude
import Set2
import Set3 (Card(Card))

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
  sequence (ma:mas) = liftM2 (:) ma (Set5.sequence mas)
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

-- Set 1

fiveRands :: Gen [Integer]
fiveRands = Set5.sequence $ replicate 5 $ Gen rand

randLetter :: Gen Char
randLetter = Set5.map (Gen rand) toLetter

randString3 :: Gen String
randString3 = do 
  a <- randLetter
  b <- randLetter
  c <- randLetter
  return $ a : b : c : ""

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair g1 g2 = do
  a <- g1
  b <- g2
  return (a,b)

-- Set 2
queryGreek :: GreekData -> String -> Maybe Double
queryGreek d key = do
  xs <- lookupMay key d
  tail <- tailMay xs
  maxTail <- maximumMay tail
  head <- headMay xs
  divMay (fromInteger maxTail) (fromInteger head)

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries d p1 p2 = do 
  s1 <- lookupMay p1 d
  s2 <- lookupMay p2 d
  return $ s1 + s2

tailProd :: Num a => [a] -> Maybe a
tailProd as = do
  tail <- tailMay as
  return $ product tail

tailSum :: Num a => [a] -> Maybe a
tailSum as = do 
  tail <- tailMay as 
  return $ sum tail

tailMax :: Ord a => [a] -> Maybe a
tailMax as = tailMay as >>= maximumMay

-- Set 3

allPairs :: [a] -> [b] -> [(a,b)]
allPairs as bs = do
  a <- as
  b <- bs
  return (a,b)

allCards :: [Int] -> [String] -> [Card]
allCards ranks suits = do
  r <- ranks
  s <- suits
  return $ Card r s

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f as bs cs = ap (ap (Set5.map as f) bs) cs
