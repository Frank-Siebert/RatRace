module RatRace.Util where

import Control.Comonad

import RatRace.Types

rightU2, leftU2, upU2, downU2 :: U2 a -> Maybe (U2 a)
rightU2 (U2 u) = fmap U2 . rightU $ u
leftU2  (U2 u) = fmap U2 .  leftU $ u
upU2    (U2 u) = fmap U2 . fmap' rightU $ u
downU2  (U2 u) = fmap U2 . fmap'  leftU $ u

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


data U2Graph a = U2Graph {
    _down2  :: Maybe (U2Graph a),
    _left2  :: Maybe (U2Graph a),
    _here2  :: a,
    _right2 :: Maybe (U2Graph a),
    _up2    :: Maybe (U2Graph a)
}

instance (Show a) => Show (U2Graph a) where
   show = show . _here2


-- from http://stackoverflow.com/questions/28516819/tying-the-knot-with-a-comonad
toU2Graph :: (U2Graph b -> a -> b) -> U2 a -> U2Graph b
toU2Graph c (U2 (U ls (U ds h us) rs)) = g
    where
        g = U2Graph (build u2down g ds) (build u2left g ls) (c g h) (build u2right g rs) (build u2up g us)
        build _ _    []            = Nothing
        build f prev (here:theres) = Just g'
            where
                g' = f (Just prev) here (build f g' theres)
        u2up   d h u = let g' = U2Graph d (d >>= _left2 >>= _up2  ) (c g' h) (d >>= _right2 >>= _up2  ) u in g'
        u2down u h d = let g' = U2Graph d (u >>= _left2 >>= _down2) (c g' h) (u >>= _right2 >>= _down2) u in g'
        u2left r (U ds h us) l = g'
            where
                g' = U2Graph (build u2down g' ds) l (c g' h) r (build u2up g' us)
        u2right l (U ds h us) r = g'
            where
                g' = U2Graph (build u2down g' ds) l (c g' h) r (build u2up g' us)

toU2GraphW :: (U2Graph b -> U2 a -> b) -> U2 a -> U2Graph b
toU2GraphW f u = toU2Graph f (duplicate u)

-- | untilM p action  repeats action until p is satisfied
untilM :: (Monad m) => (a -> Bool) -> m a -> m a
untilM p action = do x <- action
                     if (p x) then return x else untilM p action
