{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
-- allPairs [] _ = []
-- allPairs (a:as) bs = map (\b -> (a,b)) bs ++ allPairs as bs
allPairs = allCombs (,)

data Card = Card Int String
instance Show Card where
  show (Card rank suit) = show rank ++ suit

allCards :: [Int] -> [String] -> [Card]
-- allCards ranks suits = map (uncurry Card) $ allPairs ranks suits
-- allCards [] _ = []
-- allCards (r:rs) suits = map (\s -> Card r s) suits ++ allCards rs suits
allCards = allCombs Card

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs fab as bs = combStep (combStep [fab] as) bs
-- allCombs _ [] _ = []
-- allCombs f (a:as) bs = map (\b -> f a b) bs ++ allCombs f as bs

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
-- allCombs3 _ [] _ _ = []
-- allCombs3 f (a:as) bs cs = 
--   allCombs (f a) bs cs ++ allCombs3 f as bs cs
allCombs3 f as bs cs = combStep (combStep (combStep [f] as) bs) cs

combStep :: [a -> b] -> [a] -> [b]
combStep [] _ = []
combStep (f:fs) as = map (\a -> f a) as ++ combStep fs as
