{-# Language DeriveFunctor #-}
module RatRace.Types where

import Control.Comonad
import Control.Monad ((>=>))
import Data.Maybe (catMaybes)
import System.Random (StdGen)

type Genome = [Bool]
type Color = Int

type Vision = U2 Color

data Move = StandStill | North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving (Enum, Bounded)

getOffset :: Move -> Position
getOffset StandStill = ( 0, 0)
getOffset North      = ( 0, 1)
getOffset NorthEast  = ( 1, 1)
getOffset      East  = ( 1, 0)
getOffset SouthEast  = ( 1,-1)
getOffset South      = ( 0,-1)
getOffset SouthWest  = (-1,-1)
getOffset      West  = (-1, 0)
getOffset NorthWest  = (-1, 1)

type Player = Genome -> Run
type Run = (StdGen -> Vision -> Move)
-- TODO rename Run vs Move, something is fishy

data U a = U [a] a [a] deriving (Show,Eq,Functor)

rightU :: U a -> Maybe (U a)
rightU (U _ _ []) = Nothing
rightU (U ls x (r:rs)) = Just (U (x:ls) r rs)

leftU :: U a -> Maybe (U a)
leftU (U [] _ _) = Nothing
leftU (U (l:ls) x rs) = Just (U ls l (x:rs))

rightU2, leftU2, upU2, downU2 :: U2 a -> Maybe (U2 a)
rightU2 (U2 u) = fmap U2 . rightU $ u
leftU2  (U2 u) = fmap U2 .  leftU $ u
upU2    (U2 u) = fmap U2 . fmap' rightU $ u
downU2  (U2 u) = fmap U2 . fmap'  leftU $ u

listU :: ([a] -> [a]) -> U a -> U a
listU f (U ls x rs) = U (f ls) x (f rs)

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f z = case f z of
                Nothing -> []
                Just z' -> z':iterateMaybe f z'

rightUs :: U a -> [U a]
rightUs = iterateMaybe rightU

leftUs :: U a -> [U a]
leftUs =iterateMaybe leftU

instance Comonad U where
    extract (U _ x _) = x
    duplicate z = U (leftUs z) z (rightUs z)

newtype U2 a = U2 (U (U a)) deriving (Show,Eq,Functor)

-- seems to require infinite grids!!!
instance Comonad U2 where
    extract (U2 u) = extract . extract $ u
    duplicate (U2 u) =  fmap U2 $ U2 $ roll $ roll u where
        roll a = U (iterateMaybe (fmap' leftU) a) a (iterateMaybe (fmap' rightU) a)

fmap' :: (a -> Maybe b) -> U a -> Maybe (U b)
fmap' f (U ls x rs) = let z = f x in
   case z of
     Nothing   -> Nothing
     (Just x') -> Just $ U (catMaybes (map f ls)) x' (catMaybes (map f rs))

-- | here we mix position and offset
type Position = (Int,Int)

iter :: (Monad m) => Int -> (a -> m a) -> (a -> m a) -> a -> m a
iter n f g | n > 0     = g >=> iter (n-1) f g
           | n < 0     = f >=> iter (n-1) f g
           | otherwise = return

visionU2 :: Position -> U2 a -> Maybe (U2 a)
visionU2 (x,y) u = ((iter x leftU2 rightU2 =<< iter y downU2 upU2 u))

visionAt :: Position -> Vision -> Int
visionAt (x,y) = maybe (-1) extract . visionU2 (x,y)

view :: Move -> Vision -> Int
view dir = visionAt (getOffset dir)
