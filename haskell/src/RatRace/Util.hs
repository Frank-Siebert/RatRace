module RatRace.Util where

import Control.Comonad

import RatRace.Types


rightU2 :: U2 a -> Maybe (U2 a)
rightU2 (U2 u) = fmap U2 . rightU $ u

listU :: ([a] -> [a]) -> U a -> U a
listU f (U ls x rs) = U (f ls) x (f rs)

-- a real listU2 function would need higher rank:
--(forall a.[a] -> [a]) -> U2 a -> U2 a
takeU2 :: Int -> U2 a -> U2 a
takeU2 n (U2 (U ls x rs)) = U2 . fmap (listU (take n)) $ U (take n ls) x (take n rs)

fromListU :: [a] -> U a
fromListU (x:xs) = U [] x xs
fromListU _ = error "fromListU: Empty list"

fromListU2 :: [[a]] -> U2 a
fromListU2 (x:xs) = U2 (U [] (fromListU x) (map fromListU xs))
fromListU2 _ = error "fromListU2: Empty list"

listFromU :: U a -> [a]
listFromU (U ls x rs) = reverse ls ++ (x:rs)

listFromU2 :: U2 a -> [[a]]
listFromU2 (U2 u) = map listFromU (listFromU u)

