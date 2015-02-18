module RatRace.Controller where

import Control.Applicative
import Control.Arrow (second)
import Control.Comonad
import Control.Monad
import Control.Monad.State (evalStateT,runStateT)
import Control.Monad.Trans (lift,liftIO)
import Data.Graph.AStar (aStar)
import Data.Maybe (catMaybes)
import Data.Maybe (fromJust) -- debug
import Data.Ord (comparing)
import qualified Data.Set as Set (Set,fromList)
import Debug.Trace
import System.Random

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

data Contestant = Contestant {
   playerStrategy :: Player,
   liveRats :: [Specimen],
   score :: Double
}

iter :: (Monad m) => Int -> (a -> m a) -> (a -> m a) -> a -> m a
iter n f g | n > 0     = g >=> iter (n-1) f g
           | n < 0     = f >=> iter (n-1) f g
           | otherwise = return

moveFocus :: Position -> U2Graph a -> Maybe (U2Graph a)
moveFocus (x,y) = iter x _left2 _right2 >=> iter y _down2 _up2

buildFullCell :: [Cell] -> U2 (Position,Color) -> U2Graph FullCell
buildFullCell cards track = toU2GraphW b track where
    b this u = FullCell {
        vision   = snd <$> (takeU2 2 u),
        nextCell = if trap then Nothing else _here2 <$> nc,
        position = pos,
        cellType = ct,
        move     = move' this
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
-- StandStill | North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
        move' this StandStill = nextCell . _here2 $ this
        move' this North      = (_here2 <$>  _up2   this)              >>= nextCell
        move' this NorthEast  = (_here2 <$> (_up2   this >>= _right2)) >>= nextCell
        move' this East       = (_here2 <$>  _right2 this)             >>= nextCell
        move' this SouthEast  = (_here2 <$> (_down2 this >>= _right2)) >>= nextCell
        move' this South      = (_here2 <$>  _down2 this)              >>= nextCell
        move' this SouthWest  = (_here2 <$> (_down2 this >>= _left2))  >>= nextCell
        move' this West       = (_here2 <$>  _left2 this)              >>= nextCell
        move' this NorthWest  = (_here2 <$> (_up2   this >>= _left2))  >>= nextCell


-- instance declarations to put them in Set.Set for aStar
instance Eq FullCell where
   a == b = position a == position b
instance Ord FullCell where
   compare = comparing position
instance Show FullCell where
   show fc = show (cellType fc)++ show (position fc)

neighbors :: FullCell -> Set.Set FullCell
neighbors c = Set.fromList . catMaybes . map (move c) $ [North .. NorthWest]

mkContestant :: Player -> Contestant
mkContestant p = Contestant {
    playerStrategy = p,
    liveRats = [],
    score = 1.0
}

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
                                 (\fc -> fst (position fc) >= 49) .
                            _here2 $ track)

createRaceTrack :: Rand (U2Graph FullCell)
createRaceTrack = do
    colorU2 <- generateRaceTrack
    cells <- generateCells
    let result = buildFullCell cells colorU2
    return result
    -- TODO simple applicative / liftA2

runContest :: [Player] -> IO ()
runContest ps = newStdGen >>= evalStateT (
     do let contestants = map mkContestant ps
        genome <- lower randomGenome
        liftIO $ print genome
        liftIO $ putStrLn "Now for some more stuff"
        rt' <- lower generateRaceTrack
        liftIO $ print rt'
        cells <- lower generateCells
        liftIO $ print cells
        oneRt <- fromJust . (_right2 >=> _right2 >=> _up2) <$> lower createRaceTrack
        liftIO $ print (_here2 $ oneRt)
        liftIO $ putStrLn $ "The neighbors are :" ++ show (neighbors . _here2 $ oneRt)
        rt <- untilM checkRaceTrack (lower createRaceTrack)
        lift $ print $ checkRaceTrack rt
        score <- mapM (scoreTrack rt) contestants
        liftIO $ print $ "A player scored " ++ show score ++ "."
        )

data Specimen = Specimen { genome :: Genome, completedRuns :: Int, age :: Int, ratCell :: FullCell } --


scoreTrack :: U2Graph FullCell -> Contestant -> RandT IO Int
scoreTrack track player = do initialRats <- replicateM 10 createSpecimen
                             (score,_) <- trackTurn gameTurns initialScore initialRats
                             return score
    where
        createSpecimen :: RandT IO Specimen
        createSpecimen = Specimen <$> lower randomGenome <*> pure 0 <*> pure 0 <*> drawFromList startingCells
        startingCells = map _here2 $ admissibleStartingCells track
        trackTurn :: Int -> Int -> [Specimen] -> RandT IO (Int,[Specimen])
        trackTurn 0 score rats     = return (score,rats)
        trackTurn _ score       [] = return (score,[])
        trackTurn _ score rats@[_] = return (score,rats)
        trackTurn roundsLeft score rats = do
            let currentRats = rats
            childrenG <- breed currentRats
            children <- forM childrenG (\gen -> Specimen gen 0 0 <$> drawFromList startingCells)
            trackTurn (roundsLeft - 1) score (currentRats++children)

fitnessScore :: Specimen -> Int
fitnessScore (Specimen _ cr _ cell) = cr * 50 + fst (position cell)
-- need admissibleStartingCells, so maybe just return genome?
-- | only the children, the incoming list is not returned
breed :: [Specimen] -> RandT IO [Genome]
breed parents = let total = sum $ map  fitnessScore parents
                 in replicateM 10 $ do
                        motherFitness <- getRandomR (0, total - 1)
                        let (mother, other) = drawRat parents motherFitness
                            total' = total - fitnessScore mother
                        fatherFitness <- getRandomR (0, total' - 1)
                        let (father, _) = drawRat parents fatherFitness
                        lower $ mixGenome (genome mother) (genome father)

drawRat :: [Specimen] -> Int -> (Specimen,[Specimen])
drawRat = go id where
        go difflist (rat:pool) ness =
            let fitness = fitnessScore rat
             in if fitness > ness
                                then (rat,difflist pool)
                                else go ((rat:).difflist) pool (ness - fitness) -- TODO check if that is the right way for difflist
