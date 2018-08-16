{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show (Just a) = "Just " ++ (show a)
  show Nothing = "Nothing"

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay x ((a,b):xs) 
  | x == a = Just b
  | otherwise = lookupMay x xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay a b = Just (a / b)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay [x] = Just x
maximumMay (x:xs) = let Just y = maximumMay xs in Just $ max x y

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay [x] = Just x
minimumMay (x:xs) = let Just y = minimumMay xs in Just $ min x y

queryGreek :: GreekData -> String -> Maybe Double
queryGreek d key =
  case lookupMay key d of 
    Nothing -> Nothing
    Just xs ->
      case tailMay xs of 
        Nothing -> Nothing
        Just tail ->
          case maximumMay tail of 
            Nothing -> Nothing
            Just maxTail ->
              case headMay xs of 
                Nothing -> Nothing
                Just head -> divMay (fromInteger maxTail) (fromInteger head)

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 d key = let
  xs = lookupMay key d
  tail = flatMap xs tailMay
  head = flatMap xs headMay
  maxTail = flatMap tail maximumMay
  in flatMap (sequence (maxTail, head)) (\x -> divMay (fromInteger $ fst x) (fromInteger $ snd x))

sequence :: (Maybe a, Maybe b) -> Maybe (a,b)
sequence (Just a, Just b) = Just (a,b)
sequence _ = Nothing

fmap :: Maybe a -> (a -> b) -> Maybe b
fmap Nothing _ = Nothing
fmap (Just a) f = Just $ f a

flatMap :: Maybe a -> (a -> Maybe b) -> Maybe b
flatMap Nothing _ = Nothing
flatMap (Just a) f = f a

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flatMap

yLink :: Maybe a -> Maybe b -> (a -> b -> c) -> Maybe c
yLink ma mb fab = link ma (\a -> link mb (\b -> Just $ fab a b))

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries d p1 p2 = let 
  s1 = lookupMay p1 d
  s2 = lookupMay p2 d
  pair = sequence (s1, s2)
  in fmap pair (\(a,b) -> a + b)
  
addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 d p1 p2 = let 
  s1 = lookupMay p1 d
  s2 = lookupMay p2 d
  in yLink s1 s2 (+)

tailProd :: Num a => [a] -> Maybe a
tailProd as = let
  tail = tailMay as 
  in fmap tail product

tailSum :: Num a => [a] -> Maybe a
tailSum as = fmap (tailMay as) sum

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax as = fmap (tailMay as) (maximumMay)

flatten :: Maybe (Maybe a) -> Maybe a
flatten (Just (Just a)) = Just a
flatten _ = Nothing