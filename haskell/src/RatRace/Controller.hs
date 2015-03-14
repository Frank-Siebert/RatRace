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
import Data.Maybe (fromJust) -- debug
import Data.Ord (comparing)
import qualified Data.Set as Set (Set,fromList,delete)
import Debug.Trace
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Random (newStdGen, StdGen)

import RatRace.Types
import RatRace.Util
import RatRace.Rand

----- below here controller only

data FullCell = FullCell {
   vision   :: U2 Color,
   nextCell :: Maybe FullCell, -- Nothing means specimen dies
   position :: Position,
   cellType :: Cell,
   move     :: Move -> Maybe FullCell
}

iter :: (Monad m) => Int -> (a -> m a) -> (a -> m a) -> a -> m a
iter n f g | n > 0     = g >=> iter (n-1) f g
           | n < 0     = f >=> iter (n-1) f g
           | otherwise = return

moveFocus :: Position -> U2Graph a -> Maybe (U2Graph a)
moveFocus (x,y) = iter x _left2 _right2 >=> iter y _down2 _up2

visionU2 :: Position -> U2 a -> Maybe (U2 a)
visionU2 (x,y) u = ((iter x leftU2 rightU2 =<< iter y downU2 upU2 u))

visionAt :: Position -> Vision -> Int
visionAt (x,y) = maybe (-1) extract . visionU2 (x,y)

view :: Move -> Vision -> Int
view dir = visionAt (getOffset dir)

buildFullCell :: [Cell] -> U2 (Position,Color) -> U2Graph FullCell
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

checkRaceTrack :: U2Graph FullCell -> Bool
checkRaceTrack = (>=10) . length . admissibleStartingCells

admissibleStartingCells :: U2Graph FullCell -> [U2Graph FullCell]
admissibleStartingCells raceTrack = filter isAdmissibleStartingCell (raceTrack : (iterateMaybe _up2 $ raceTrack))

-- | for a single
isAdmissibleStartingCell :: U2Graph FullCell -> Bool
isAdmissibleStartingCell track = maybe False ((<=100) . length) (
                           aStar neighbors
                                 (\_ _ -> 1)
                                 (\fc -> (fromInteger . toInteger) (abs (50 - fst (position fc))) / (4.0 :: Double))
                                 isGoal .
                            _here2 $ track)

isGoal :: FullCell -> Bool
isGoal = (>=49) . fst . position

createRaceTrack :: Int -> Int -> Rand (U2Graph FullCell)
createRaceTrack l w = buildFullCell <$> generateCells <*> generateRaceTrack l w
--createRaceTrack = buildFullCell <$> pure (replicate 16 (Teleporter 0 0)) <*> generateRaceTrack

options :: [OptDescr (SimulationOptions -> SimulationOptions)]
options =
    [ Option ['g','r']     ["games","rounds"]
        (ReqArg (\x opts -> opts { rounds = read x }) "rounds?")
        "rounds"
    , Option ['t'] ["turns"]
        (ReqArg (\x opts -> opts { gameTurns = read x }) "turns?")
        "show version number"
    ]

compilerOpts :: [String] -> SimulationOptions
compilerOpts argv =
    case getOpt Permute options argv of
         (o,_,[]  ) -> foldl (flip id) defaultOptions o
         (_,_,errs) -> error (concat errs ++ usageInfo "RatRace" options)

runContest :: [Player] -> IO ()
runContest ps =
  do argv <- getArgs
     let config :: SimulationOptions
         config = compilerOpts argv
     putStrLn $ "With options " ++ show config
     gs <- take (rounds config) . randomGens <$> newStdGen
     let results :: [[Int]]
         results = parMap rdeepseq (runGame config ps) gs
     putStrLn $ "The players scored " ++ (unlines . map show $ results ) ++ "."
     let scores = map geometricMean . transpose $ results
     putStrLn $ "Final scores: " ++ show scores

geometricMean :: [Int] -> Double
geometricMean xs = exp . (/ (fromInteger . toInteger . length) xs) . sum . map (log . fromInteger . toInteger) $ xs

runGame :: SimulationOptions -> [Player] -> StdGen -> [Int]
runGame config ps = evalState $
    do rt <- untilM checkRaceTrack (createRaceTrack (raceTrackLength config) (raceTrackWidth config))
       mapM (scoreTrack config rt) ps

data Specimen = Specimen { genome :: !Genome, completedRuns :: !Int, age :: !Int, run :: !Run, ratCell :: FullCell } --

scoreTrack :: SimulationOptions -> U2Graph FullCell -> Player -> Rand Int
scoreTrack config track player = replicateM (initialRatCount config) randomGenome >>=
                          mapM createSpecimen >>=
                          trackTurn (gameTurns config) (initialScore config)
    where
        createSpecimen :: Genome -> Rand Specimen
        createSpecimen genome = do Specimen genome 0 0 (player genome) <$> drawFromList startingCells
        startingCells = map _here2 $ admissibleStartingCells track
        trackTurn :: Int -> Int -> [Specimen] -> Rand Int
        trackTurn _  (-1) _        = return (-1) -- cannot happen, but makes score strict
        trackTurn 0 score rats     = return score
        trackTurn _ score       [] = return score
        trackTurn _ score rats@[_] = return score
        trackTurn roundsLeft score rats = do
            let youngRats = filter ((<100) . age) rats
            movedRats <- catMaybes <$> mapM moveSpecimen youngRats
            scoredRats <- mapM resetScorer movedRats
            let turnScore = sum . map snd $ scoredRats
            let currentRats = map fst $ scoredRats
            children <- (breed currentRats >>= mapM mutateGenome >>= mapM createSpecimen)
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
-- need admissibleStartingCells, so maybe just return genome?
-- | only the children, the incoming list is not returned
breed :: [Specimen] -> Rand [Genome]
breed  [] = return []
breed [_] = return []
breed parents = let total = (sum $ map  fitnessScore parents) - 1
                 in replicateM 10 $ do
                        motherFitness <- getRandomR (0, total)
                        let (mother, partial) = drawRat parents motherFitness
                            total' = total - fitnessScore mother
                        fatherFitness <- getRandomR (0, total')
                        let fatherFitness' = if fatherFitness >=partial then fatherFitness + fitnessScore mother else fatherFitness
                        let (father, _) = drawRat parents fatherFitness
                        mixGenome (genome mother) (genome father)

drawRat :: [Specimen] -> Int -> (Specimen,Int)
drawRat = go 0 where
          go s (rat:pool) ness =
            let fitness = fitnessScore rat
             in if fitness > ness
                    then (rat,s)
                    else go (s + fitness) pool (ness - fitness)
          go f [] ness = error $ "Ness is still " ++ show ness ++ " at empty list; " ++ show f
