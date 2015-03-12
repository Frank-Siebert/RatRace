import RatRace.RatRace

import RatRace.Util -- for square, remove, RatRace.RatRace import should be enough
import RatRace.Types
import System.Random
import RatRace.Controller
import Control.Comonad
import Data.List (maximumBy,transpose)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)


main :: IO ()
--main = runContest [myPlayer,colorScoringPlayer unaryScoring, colorScoringPlayer binaryScoring, blind West]
main = runContest [myPlayer]

square :: U2 Char
square = fromListU2 ["abc","def","ghi"]

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
binaryScoring = go 0 where
                go (-9) [] = 0 -- strictness
                go acc [] = acc
                go acc (x:xs) = go (fromEnum x + 2*acc) xs

colorScoringPlayer :: Scoring -> Player
colorScoringPlayer scoring genome =
    let scores :: [Int]
        scores = map scoring . transpose . chunksOf 16 . take 96 $ genome
        score (-1) = -99
        score   x  = scores !! x
     in \g v -> fst . maximumBy (comparing snd ) . map (\x -> (x, score $ view x v)) $ forward

data Trap = T Int Position
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
                 in fst . maximumBy (comparing snd ) . map (\x -> (x, view x (fst <$> v'))) $ forward
