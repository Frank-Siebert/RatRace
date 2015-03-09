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
main = runContest [colorScoringPlayer unaryScoring, colorScoringPlayer binaryScoring, blind West]

square :: U2 Char
square = fromListU2 ["abc","def","ghi"]

single = U2 (U [] (U "" 'X' "") [])

blind :: Move -> Player
blind x genome = \_ _ -> x


forward = [NorthEast,East,SouthEast]

type Scoring = [Bool] -> Int

unaryScoring :: Scoring
unaryScoring = length . filter id

binaryScoring :: Scoring
binaryScoring = go 0 where
                go (-9) [] = 0 -- strictness
                go acc [] = acc
                go acc (x:xs) = go (fromEnum x + 2*acc) xs

colorScoringPlayer :: Scoring -> Player
colorScoringPlayer scoring genome =
    let scores :: [Int]
        scores = map (length . filter id) . transpose . chunksOf 16 . take 96 $ genome
        score (-1) = -99
        score   x  = scores !! x
     in \g v -> fst . maximumBy (comparing snd ) . map (\x -> (x, score $ view x v)) $ forward
