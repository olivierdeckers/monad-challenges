{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

type Gen a = Seed -> (a, Seed)

fiveRands :: [Integer]
fiveRands = take 5 $ rands (mkSeed 1)

rands :: Seed -> [Integer]
rands s = 
  let 
    (i, s2) = rand s
  in 
    i : (rands s2)

randLetter :: Gen Char
-- randLetter s = 
--   let (i, s2) = rand s
--   in (toLetter i, s2)
randLetter = generalA rand toLetter

randLetters :: Seed -> [Char]
randLetters s = 
  let 
    (i, s2) = randLetter s
  in 
    i : (randLetters s2)


randString3:: String
randString3 = take 3 $ randLetters (mkSeed 1)

randEven :: Gen Integer
randEven = generalA rand (\x -> x * 2)

randOdd :: Gen Integer
randOdd = generalA randEven (\x -> 1 + x)

randTen :: Gen Integer
randTen = generalA rand (\x -> 10 * x)

generalA :: Gen a -> (a -> b) -> Gen b
generalA gen f s =
  let 
    (a, s2) = gen s
    b = f a
  in (b, s2)

randPair :: Gen (Char, Integer)
randPair s = let 
    (c, s2) = randLetter s
    (i, s3) = rand s2
  in ((c, i), s3)

generalPair  :: Gen a -> Gen b -> Gen (a,b)
-- generalPair g1 g2 = \s -> let 
--   (a, s2) = g1 s
--   (b, s3) = g2 s2
--   in ((a, b), s3)
generalPair = generalB (\(a, b) -> (a,b))

generalB :: ((a,b) -> c) -> Gen a -> Gen b -> Gen c
generalB f g1 g2 = \s -> let
  (a, s2) = g1 s
  (b, s3) = g2 s2
  in (f(a,b), s3)

generalB2 :: ((a,b) -> c) -> Gen a -> Gen b -> Gen c
generalB2 f g1 g2 = genTwo g1 (\a -> genTwo g2 (\b -> mkGen $ f (a,b)))

repRandom :: [Gen a] -> Gen [a]
-- repRandom [] = \s -> ([], s)
-- repRandom (g : gs) = \s -> let 
--   (x, s2) = g s
--   (xs, s3) = repRandom gs s2
--   in (x : xs, s3)
repRandom [] = mkGen []
repRandom (g:gs) = generalB (\(a,b) -> a : b) g (repRandom gs)

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo ga fab = \s -> let
  (a, s2) = ga s
  in fab a s2

mkGen :: a -> Gen a
mkGen a s = (a, s)