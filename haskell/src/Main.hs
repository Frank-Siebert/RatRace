import RatRace.RatRace
import RatRace.Util (listFromU2)

import System.Random
import Control.Comonad
import Data.List (foldl',maximumBy,transpose)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))


main :: IO ()
--main = runContest [myPlayer, colorScoringPlayer unaryScoring, colorScoringPlayer binaryScoring, colorScoringPlayer' unaryScoring, colorScoringPlayer' binaryScoring, blind West]
main = runContest [myPlayer2 quot, myPlayer2 rem, myPlayer, colorScoringPlayer binaryScoring, colorScoringPlayer' binaryScoring]

blind :: Move -> Player
blind x _ = \_ _ -> x

forward, forward' :: [Move]
forward = [NorthEast,East,SouthEast]
forward' = reverse forward

type Scoring = [Bool] -> Int

{-# INLINE unaryScoring #-}
unaryScoring :: Scoring
unaryScoring = length . filter id

{-# INLINE binaryScoring #-}
binaryScoring :: Scoring
binaryScoring = foldl' (\x y -> 2*x + fromEnum y) 0

takeBits :: Int -> Int -> Int -> Genome -> Int
takeBits from step count gen = binaryScoring . map (gen !) $ [from,from+step..from+step*(count-1)]

colorScoringPlayer :: Scoring -> Player
colorScoringPlayer scoring genome =
    let scores :: V.Vector Int
        scores = V.generate 16 (\i -> takeBits i 16 6 genome)
        score (-1) = -99
        score   x  = scores ! x
     in \g v -> takeOne g . map (\x -> (x, score $ view x v)) $ forward

colorScoringPlayer' :: Scoring -> Player
colorScoringPlayer' scoring genome =
    let scores :: V.Vector Int
        scores = V.generate 16 (\i -> takeBits (i*6) 1 6 genome)
        score (-1) = -99
        score   x  = scores ! x
     in \g v -> takeOne g . map (\x -> (x, score $ view x v)) $ forward

data Trap = T {-# UNPACK #-} !Int {-# UNPACK #-} !Offset
mkTrap :: [Bool] -> Trap
mkTrap genome = T (binaryScoring . take 4 $ genome) (toOffset . drop 4 $ genome,toOffset . drop 8 $ genome) where
    toOffset g = let n = binaryScoring (take 4 g)
                  in n `rem` 3 -1

myPlayer :: Player
myPlayer genome =
    let scores :: V.Vector Int
        scores = V.generate 16 (\i -> takeBits (i*4) 1 4 genome)
        score (-1) = -99
        score   x  = scores ! x
        trap1      = mkTrap (V.toList $ V.drop 64 genome)
        trap2      = mkTrap (V.toList $ V.drop 86 genome)
     in \g v -> let v'    = ((\x -> (score x,x)) <$> v) =>> pulldown trap1 =>> pulldown trap2
                    pulldown :: Trap -> (U2 (Int,Int)) -> (Int,Int)
                    pulldown (T c p) u = case extract <$> visionU2 p u of
                       Just (_,c') | c==c' -> (-50,c)
                       _                   ->  extract u
                 in takeOne g . map (\x -> (x, 8* fst (getOffset x)+view x (fst <$> v'))) $ [North,NorthEast,East,SouthEast,South]

takeOne :: StdGen -> [(Move,Int)] -> Move
takeOne g = rnd . map fst . maximaBy (comparing snd) where
    rnd :: [a] -> a
    rnd [x] = x
    rnd xs = xs !! fst (randomR (0, length xs - 1) g)

maximaBy :: (a -> a -> Ordering) -> [a] -> [a]
maximaBy _ [] = []
maximaBy c (x:xs) = case maximaBy c xs of
                        []       -> [x]
                        ys@(y:_) -> case c x y of
                                       EQ -> x:ys
                                       LT -> ys
                                       GT -> [x]

myPlayer2 :: (Int -> Int -> Int) -> Player
myPlayer2 (#) genome =
    let scores :: V.Vector Int
        scores = V.generate 16 (\i -> takeBits (i*5) 1 5 genome)
        score (-1) = -99
        score   x  = scores ! x
     in \_ v -> let ls = concat . listFromU2 $ v
                    hash = foldl' (\a b -> (a*3 + b) # 20) 0 ls
                    xward = if genome ! (80 + hash) then forward else forward'
                 in fst . head . maximaBy (comparing snd) . map (\x -> (x, score $ view x v)) $ xward
