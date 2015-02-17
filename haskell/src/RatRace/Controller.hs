module RatRace.Controller where

import Control.Applicative
import Control.Arrow (second)
import Control.Comonad
import Control.Monad
import Control.Monad.State (evalState) -- for debug only?
import Data.Graph.AStar (aStar)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Set as Set (Set,fromList)
import System.Random

import RatRace.Types
import RatRace.Util
import RatRace.Rand



----- below here controller only

data Specimen = Specimen { genome :: Genome, completedRuns :: Int, age :: Int, ratCell :: FullCell } --

data FullCell = FullCell {
   vision   :: U2 Color,
   nextCell :: Maybe (U2Graph FullCell), -- Nothing means specimen dies
   position :: Position,
   cellType :: Cell,
   move     :: Move -> Maybe (U2Graph FullCell)
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
        nextCell = if trap then Nothing else nc,
        position = pos,
        cellType = ct,
        move     = undefined
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


-- instance declarations to put them in Set.Set for aStar
instance Eq FullCell where
   a == b = position a == position b
instance Ord FullCell where
   compare = comparing position

neighbors :: FullCell -> Set.Set FullCell
neighbors c = Set.fromList . map _here2 . catMaybes . map (move c) $ [North .. NorthWest]

runContest :: [Player] -> IO ()
runContest _ =
     do newStdGen >>= print  . evalState randomGenome
        putStrLn "Now for some more stuff"
        newStdGen >>= print . evalState generateRaceTrack
        newStdGen >>= print . evalState generateCells

