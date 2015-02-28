module RatRace.Rand (
    module RatRace.Rand,
    evalStateT,
    runStateT
) where

import Control.Applicative
import Control.Monad.State.Strict
import qualified Control.Monad.Identity
import System.Random

import RatRace.Types
import RatRace.Util

type RandT m = StateT StdGen m
type Rand = RandT Control.Monad.Identity.Identity

lower :: (Monad m) => Rand a -> RandT m a
lower action = StateT $ \s -> return (runState action s)

randomGenome :: Rand Genome
randomGenome = replicateM 100 getRandom

getRandom :: (Random a) => Rand a
getRandom = do (x,g) <- (random <$> get)
               g `seq` put g
               x `seq` return x

getRandomR :: (Random a, Monad m, Functor m) => (a,a) -> RandT m a
getRandomR range = do (x,g) <- (randomR range <$> get)
                      g `seq` put g
                      x `seq` return x

getStdGen :: (Monad m, Functor m) => RandT m StdGen
getStdGen = do (g,h) <- (split <$> get)
               put g
               return h

mixGenome :: Genome -> Genome -> Rand Genome
mixGenome mother father = do coin <- getRandom
                             mix (if coin
                                   then (mother,father)
                                   else (father,mother))
                          where
                             mix ([],recessive) = return recessive
                             mix (d:ominant,_:recessive) = (d:) <$>
                                     do x <- getRandom
                                        mix (if (x < genomeChangeChance)
                                               then (recessive,ominant)
                                               else (ominant,recessive))

mutateGenome :: Genome -> Rand Genome
mutateGenome [] = return []
mutateGenome (g:gs) = do c <- getRandom
                         ((if c < genomeFlipChance then not g else g):) <$>  mutateGenome gs

addPos :: [[a]] -> [[(Position,a)]]
addPos = zipWith (\x -> map (\(y,a)->((x,y),a))) [0..] . map (zip [0..])

generateRaceTrack :: Rand (U2 (Position,Color))
generateRaceTrack = fromListU2 . addPos <$> replicateM raceTrackLength (replicateM raceTrackWidth generateColor)

generateColor :: Rand Color
generateColor = getRandomR (0,15)

drawFromList :: (Monad m, Functor m) => [a] -> RandT m a
drawFromList xs = (xs !!) <$> getRandomR (0, length xs - 1)

generateCells :: Rand [Cell]
generateCells = do te1 <- genCell Teleporter 4
                   te2 <- genCell Teleporter 4
                   tr1 <- liftA2 Trap (getRandomR (-1,1)) (getRandomR (-1,1))
                   tr2 <- liftA2 Trap (getRandomR (-1,1)) (getRandomR (-1,1))
                   shuffle $ [Wall,Wall,te1,te2,invertCell te1, invertCell te2, tr1,tr2]++replicate 8 emptyCell where
           genCell ctor r = dropWhileM (==ctor 0 0) $ liftA2 ctor (getRandomR (-r,r)) (getRandomR (-r,r))
           invertCell (Teleporter a b) = Teleporter (-a) (-b)
           invertCell c = error "invertCell on a non-Teleporter"

dropWhileM :: (Monad m) => (a -> Bool) -> m a -> m a
dropWhileM p action = do x <- action
                         if p x then dropWhileM p action else return x

shuffle :: [a] -> Rand [a]
shuffle xs = go (length xs) xs where
             go 1 a = return a
             go n as = do i <- getRandomR (0,n - 1)
                          let remainder = take i as ++ drop (i+1) as
                          res <- go (n-1) remainder
                          return $ (as!!i):res

{- orphan, I do not really need you
instance (Random a, Random b) => Random (a,b) where
    random g = let (a,g' ) = random g
                   (b,g'') = random g' in ((a,b),g'')
    randomR ((la,lb),(ha,hb)) g = let (a,g' ) = randomR (la,ha) g
                                      (b,g'') = randomR (lb,hb) g' in ((a,b),g'')
-}
