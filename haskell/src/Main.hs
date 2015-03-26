import RatRace.RatRace

import System.Random
import Control.Comonad
import Data.List (foldl',maximumBy,transpose)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)


main :: IO ()
--main = runContest [myPlayer, colorScoringPlayer unaryScoring, colorScoringPlayer binaryScoring, colorScoringPlayer' unaryScoring, colorScoringPlayer' binaryScoring, blind West]
main = runContest [myPlayer, colorScoringPlayer binaryScoring, colorScoringPlayer' binaryScoring]

single = U2 (U [] (U "" 'X' "") [])

blind :: Move -> Player
blind x genome = \_ _ -> x


forward = [NorthEast,East,SouthEast]

type Scoring = [Bool] -> Int

{-# INLINE unaryScoring #-}
unaryScoring :: Scoring
unaryScoring = length . filter id

{-# INLINE binaryScoring #-}
binaryScoring :: Scoring
binaryScoring = foldl' (\x y -> 2*x + fromEnum y) 0

colorScoringPlayer :: Scoring -> Player
colorScoringPlayer scoring genome =
    let scores :: [Int]
        scores = map scoring . transpose . chunksOf 16 . take 96 $ genome
        score (-1) = -99
        score   x  = scores !! x
     in \g v -> takeOne g . map (\x -> (x, score $ view x v)) $ forward

colorScoringPlayer' :: Scoring -> Player
colorScoringPlayer' scoring genome =
    let scores :: [Int]
        scores = map scoring . chunksOf 6 . take 96 $ genome
        score (-1) = -99
        score   x  = scores !! x
     in \g v -> takeOne g . map (\x -> (x, score $ view x v)) $ forward

data Trap = T {-# UNPACK #-} !Int {-# UNPACK #-} !Offset
mkTrap :: [Bool] -> Trap
mkTrap genome = T (binaryScoring . take 4 $ genome) (toOffset . drop 4 $ genome,toOffset . drop 8 $ genome) where
    toOffset g = let n = binaryScoring (take 4 g)
                  in n `rem` 3 -1

myPlayer :: Player
myPlayer genome =
    let scores :: [Int]
        scores = map binaryScoring . chunksOf 4 . take 64 $ genome
        score (-1) = -99
        score   x  = scores !! x
        trap1      = mkTrap (drop 64 genome)
        trap2      = mkTrap (drop 86 genome)
     in \g v -> let v'    = ((\x -> (score x,x)) <$> v) =>> pulldown trap1 =>> pulldown trap2
                    pulldown :: Trap -> (U2 (Int,Int)) -> (Int,Int)
                    pulldown (T c p) u = case extract <$> visionU2 p u of
                       Just (_,c) -> (-50,c)
                       _          ->  extract u
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
