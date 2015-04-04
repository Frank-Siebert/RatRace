module RatRace.Controller where

import Control.Applicative
import Control.Arrow (second)
import Control.Comonad
import Control.Monad
import Control.Monad.Trans (lift,liftIO)
import Control.Parallel.Strategies
import Data.Graph.AStar (aStar)
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Set as Set (Set,fromList,delete)
import System.Environment (getArgs)
import System.Random (newStdGen, StdGen, mkStdGen)

import RatRace.Options
import RatRace.Rand
import RatRace.Types
import RatRace.Util

----- below here controller only

data FullCell = FullCell {
   vision   :: U2 Color,
   nextCell :: Maybe FullCell, -- Nothing means specimen dies
   position :: Offset,
   cellType :: Cell,
   move     :: Move -> Maybe FullCell
}

moveFocus :: Offset -> U2Graph a -> Maybe (U2Graph a)
moveFocus (x,y) = iter x _left2 _right2 >=> iter y _down2 _up2

buildFullCell :: [Cell] -> U2 (Offset,Color) -> U2Graph FullCell
buildFullCell cards track = toU2GraphW b track where
    b this u = FullCell {
        vision   = snd <$> (takeU2 2 u),
        nextCell = if trap then Nothing else _here2 <$> nc,
        position = pos,
        cellType = ct,
        move     = move'
    } where
        ct = cards !! (snd $ extract u)
        pos = fst $ extract u
        nc = case ct of
            Wall           -> Nothing
            Teleporter x y -> moveFocus (x,y) this
            _              -> Just this
        trap = any checkTrap . map (second (cards !!)) . concat . listFromU2 . takeU2 1 $ u
        checkTrap ((x,y),Trap dx dy) = x + dx == fst pos && y + dy == snd pos
        checkTrap _ = False
        move'' :: (U2Graph FullCell -> Maybe (U2Graph FullCell)) -> Maybe FullCell
        move'' f = liftM (resetIfWall this) (f this) >>= nextCell . _here2
        resetIfWall :: U2Graph FullCell -> U2Graph FullCell -> U2Graph FullCell
        resetIfWall origin dest = if cellType (_here2 dest) == Wall then origin else dest
-- StandStill | North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
        move' StandStill = nextCell . _here2 $ this
        move' North      = move'' $ _up2
        move' NorthEast  = move'' $ _up2   >=> _right2
        move' East       = move'' $            _right2
        move' SouthEast  = move'' $ _down2 >=> _right2
        move' South      = move'' $ _down2
        move' SouthWest  = move'' $ _down2 >=> _left2
        move' West       = move'' $            _left2
        move' NorthWest  = move'' $ _up2   >=> _left2


-- instance declarations to put them in Set.Set for aStar
instance Eq FullCell where
   a == b = position a == position b
instance Ord FullCell where
   compare = comparing position
instance Show FullCell where
   show fc = show (cellType fc)++ show (position fc)

neighbors :: FullCell -> Set.Set FullCell
neighbors c = Set.delete c . Set.fromList . catMaybes . map (move c) $ [North .. NorthWest]

checkRaceTrack :: Int -> U2Graph FullCell -> Bool
checkRaceTrack x = (>=10) . length . admissibleStartingCells x

admissibleStartingCells :: Int -> U2Graph FullCell -> [U2Graph FullCell]
admissibleStartingCells x raceTrack = filter (isAdmissibleStartingCell x) (raceTrack : (iterateMaybe _up2 $ raceTrack))

-- | for the focused cell
isAdmissibleStartingCell :: Int -> U2Graph FullCell -> Bool
isAdmissibleStartingCell x = maybe False ((<=x) . length) . findGoalCell

findGoalCell :: U2Graph FullCell -> Maybe [FullCell]
findGoalCell = aStar neighbors
                     (\_ _ -> 1)
                     (\fc -> (fromInteger . toInteger) (abs (50 - fst (position fc))) / (4.0 :: Double))
                     isGoal .
                _here2

isGoal :: FullCell -> Bool
isGoal = (>=49) . fst . position

createRaceTrack :: Int -> Int -> Rand (U2Graph FullCell)
createRaceTrack l w = buildFullCell <$> generateCells <*> generateRaceTrack l w
--createRaceTrack = buildFullCell <$> pure (replicate 16 (Teleporter 0 0)) <*> generateRaceTrack

runContest :: [Player] -> IO ()
runContest ps =
  do argv <- getArgs
     let config :: SimulationOptions
         config = compilerOpts argv
     putStrLn $ "With options " ++ show config
     gs <- take (rounds config) . randomGens <$> maybe newStdGen (return . mkStdGen) (randSeed config)
     forM_ gs (visualizeTrack config)
     let results :: [[Int]]
         results = parMap rdeepseq (runGame config ps) gs
     putStrLn $ "The players scored " ++ (unlines . map show $ results ) ++ "."
     let scores = map geometricMean . transpose $ results
     putStrLn $ "Final scores: " ++ show scores

geometricMean :: [Int] -> Double
geometricMean xs = exp . (/ (fromInteger . toInteger . length) xs) . sum . map (log . fromInteger . toInteger) $ xs

runGame :: SimulationOptions -> [Player] -> StdGen -> [Int]
runGame config ps = evalState $
    do rt <- untilM (checkRaceTrack (maxAge config)) (createRaceTrack (raceTrackLength config) (raceTrackWidth config))
       mapM (scoreTrack config rt) ps

visualizeTrack :: SimulationOptions -> StdGen -> IO ()
visualizeTrack config g = let
        raceTrack = evalState (untilM (checkRaceTrack (maxAge config)) (createRaceTrack (raceTrackLength config) (raceTrackWidth config))) g
        paths = map (fmap length . findGoalCell) $ raceTrack : (iterateMaybe _up2 $ raceTrack)
        startPos = reverse $ raceTrack : (iterateMaybe _up2 $ raceTrack)
        line x = x : (iterateMaybe _right2 $ x)
        showCell :: Int -> FullCell -> Char
        showCell 0 c = case cellType c of
                       Wall                      -> 'W'
                       _ | nextCell c == Nothing -> 'o'
                       Teleporter x y
                         | x > 0 &&   x  >= abs y -> '>'
                         | x < 0 && (-x) >= abs y -> '<'
                         | y < 0 -> '^'
                         | y > 0 -> 'v'
                       _ -> '.'
        showCell 1 c = let col = extract . vision $ c in toEnum (col + if col < 10 then 48 else 87)
        showCell 2 c = case cellType c of
            Teleporter 0 0 -> '.'
            Teleporter _ _ -> 'T'
            Trap       _ _ -> 'X'
            Wall           -> 'W'
        showLine mode x = putStrLn . map (showCell mode . _here2)  . line $ x
    in do
        putStrLn $ "Track " ++ show g
        putStrLn $ "Paths " ++ show paths
        forM_ startPos (showLine 0)
        forM_ startPos (showLine 1)
        forM_ startPos (showLine 2)

data Specimen = Specimen { genome :: !Genome, completedRuns :: {-# UNPACK #-} !Int, age :: {-# UNPACK #-} !Int, run :: !Run, ratCell :: FullCell } --

scoreTrack :: SimulationOptions -> U2Graph FullCell -> Player -> Rand Int
scoreTrack config track player = replicateM (initialRatCount config) randomGenome >>=
                          mapM createSpecimen >>=
                          trackTurn (gameTurns config) (initialScore config)
    where
        createSpecimen :: Genome -> Rand Specimen
        createSpecimen gen = Specimen gen 0 0 (player gen) <$> drawFromList startingCells
        flipChance = (genomeFlipChance config)
        startingCells = map _here2 $ admissibleStartingCells (maxAge config) track
        trackTurn :: Int -> Int -> [Specimen] -> Rand Int
        trackTurn _           (-1)    _ = return (-1) -- cannot happen, but makes score strict
        trackTurn 0          score    _ = return score
        trackTurn _          score   [] = return score
        trackTurn _          score  [_] = return score
        trackTurn roundsLeft score rats = do
            let youngRats = filter ((<maxAge config) . age) rats
            movedRats <- catMaybes <$> mapM moveSpecimen youngRats
            scoredRats <- mapM resetScorer movedRats
            let turnScore = sum . map snd $ scoredRats
            let currentRats = map fst $ scoredRats
            children <- (breed (genomeChangeChance config) currentRats >>= mapM (mutateGenome flipChance) >>= mapM createSpecimen)
            trackTurn (roundsLeft - 1) (score + turnScore) (children ++ currentRats)
        moveSpecimen :: Specimen -> Rand (Maybe Specimen)
        moveSpecimen rat =
            do g <- getStdGen
               let newPos = moveRat g rat
               case newPos of
                  Nothing -> return Nothing
                  Just (newPos') -> return . Just $ rat { ratCell = newPos', age = age rat + 1 }
        resetScorer :: Specimen -> Rand (Specimen,Int)
        resetScorer rat = if isGoal (ratCell rat)
                               then do newPos <- drawFromList startingCells
                                       let rat' = rat { ratCell = newPos, age = 0, completedRuns = 1 + completedRuns rat }
                                       return (rat',1)
                               else return (rat,0)

moveRat :: StdGen -> Specimen -> Maybe FullCell
moveRat g rat = let cell = ratCell rat
                 in move cell $ (run rat) g (vision cell)

fitnessScore :: Specimen -> Int
fitnessScore rat = 1 + completedRuns rat * 50 + fst (position $ ratCell rat)

-- | genomeFlipChance, parents; return bred genomes without including parents
breed :: Double -> [Specimen] -> Rand [Genome]
breed _  [] = return []
breed _ [_] = return []
breed c parents = let total = (sum $ map  fitnessScore parents) - 1
                 in replicateM 10 $ do
                        motherFitness <- getRandomR (0, total)
                        let (mother, partial) = drawRat parents motherFitness
                            total' = total - fitnessScore mother
                        fatherFitness <- getRandomR (0, total')
                        let fatherFitness' = if fatherFitness >=partial then fatherFitness + fitnessScore mother else fatherFitness
                        let (father, _) = drawRat parents fatherFitness'
                        mixGenome c (genome mother) (genome father)

drawRat :: [Specimen] -> Int -> (Specimen,Int)
drawRat = go 0 where
          go s (rat:pool) ness =
            let fitness = fitnessScore rat
             in if fitness > ness
                    then (rat,s)
                    else go (s + fitness) pool (ness - fitness)
          go f [] ness = error $ "Ness is still " ++ show ness ++ " at empty list; " ++ show f
