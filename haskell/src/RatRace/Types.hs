{-# Language DeriveFunctor #-}
module RatRace.Types where

import Control.Comonad
import Data.Maybe (catMaybes)
import System.Random (StdGen)

raceTrackLength :: Int
raceTrackLength = 54

raceTrackWidth :: Int
raceTrackWidth = 15

genomeLength :: Int
genomeLength = 100

genomeChangeChance :: Double
genomeChangeChance = 0.05

genomeFlipChance :: Double
genomeFlipChance = 0.01


type Genome = [Bool]
type Color = Int

type Vision = U2 Color

data Move = StandStill | North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving (Enum)


type Player = Genome -> (StdGen -> Vision -> Move)


data U a = U [a] a [a] deriving (Show,Eq,Functor)

rightU :: U a -> Maybe (U a)
rightU (U _ _ []) = Nothing
rightU (U ls x (r:rs)) = Just (U (x:ls) r rs)

leftU :: U a -> Maybe (U a)
leftU (U [] _ _) = Nothing
leftU (U (l:ls) x rs) = Just (U ls l (x:rs))

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
        fmap' :: (a -> Maybe b) -> U a -> Maybe (U b)
        fmap' f (U ls x rs) = let z = f x in
           case z of
             Nothing   -> Nothing
             (Just x') -> Just $ U (catMaybes (map f ls)) x' (catMaybes (map f rs))
        roll a = U (iterateMaybe (fmap' leftU) a) a (iterateMaybe (fmap' rightU) a)
    
-- technically, Empty == Teleporter 0 0
data Cell = Wall | Teleporter Int Int | Trap Int Int deriving (Eq,Show)
emptyCell :: Cell
emptyCell = Teleporter 0 0

type Position = (Int,Int)
