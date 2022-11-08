module Lab2 where

import Prelude

import Data.List (List(..), reverse, filter, (:))
import Data.List (List(..), length, reverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)



findIndex' :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex' predicate = go 0
  where
  go :: Int -> List a -> Maybe Int
  go n (Cons x xs) | predicate x = Just n
                   | otherwise = go (n + 1) xs
  go _ Nil = Nothing
  
findLastIndex' :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex' fn xs = go (length(xs) - 1) (reverse(xs))
  where
    go :: Int -> List a -> Maybe Int
    go n (Cons a as) | fn a = Just n
                     | otherwise = go (n - 1) as
    go _ Nil = Nothing


zip' :: forall a b. List a -> List b -> List (Tuple a b)
zip' xs ys = reverse $ go xs ys Nil
  where
  go Nil _ acc = acc
  go _ Nil acc = acc
  go (Cons x xs) (Cons b ys) acc = go xs ys $ Cons (Tuple x b) acc


unzip' :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip' ab = go ab Nil Nil
  where
  go Nil accx accy = Tuple (reverse accx) (reverse accy)
  go (Cons x xs) Nil Nil = go xs (Cons (fst x) Nil) (Cons (snd x) Nil)
  go (Cons x xs) accx accy = go xs (Cons (fst x) accx) (Cons (snd x) accy)


filter' :: forall a. (a -> Boolean) -> List a -> List a
filter' _ Nil = Nil
filter' predicate (Cons x xs) | predicate x = Cons x $ filter predicate xs
                     | otherwise = filter predicate xs

tailRecursionFilter' :: forall a. (a -> Boolean) -> List a -> List a
tailRecursionFilter' predicate list = go Nil list
  where 
  go acc Nil = reverse acc
  go acc (x : xs)
    |predicate x = go(x : acc) xs
    |otherwise = go acc xs
    

take' :: forall a. Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' n (Cons x xs) = Cons x $ take' (n - 1) xs


tailRecursionTake' :: forall a. Int -> List a -> List a
tailRecursionTake' = go Nil
  where
  go acc _ Nil = reverse acc
  go acc 0 _ = reverse acc
  go acc n (Cons x xs) = go (Cons x acc) (n - 1) xs


test::Effect Unit
test = do 

  let mylist = (Cons 1 (Cons 1(Cons 3 Nil)))
  let mylist2 = (Cons 2 (Cons 2(Cons 4 Nil)))
  let mylist3 =(Cons 217 (Cons 2(Cons 2(Cons 4 Nil))))

  logShow $ "findIndex"
  logShow $ findIndex'(_==1) mylist
  
  logShow $ "findLastIndex"
  logShow $ findLastIndex'(_==1) mylist
  
  logShow $ "Zip"
  let zipped = zip' mylist mylist2
  
  logShow $ zipped
  
  logShow $ "First unzip"
  
  logShow $ fst $ unzip' zipped
  
  logShow $ "Second unzip"
  logShow $ snd $ unzip' zipped
  
  logShow $ "Filter"
  logShow $ filter'(_<2) mylist
  
  logShow $ "tailRecursionFilter"
  logShow $ tailRecursionFilter'(_<2) mylist
  
  logShow $ "take"
  logShow $ take'(3) mylist3
  
  logShow $ "tailRecursionTake"
  logShow $ tailRecursionTake'(3) mylist3

