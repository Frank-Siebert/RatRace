module RatRace.Rand (
    module RatRace.Rand,
    evalState
) where

import Control.Applicative
import Control.Monad.State.Strict
import System.Random

import RatRace.Types
import RatRace.Util

type Rand = State StdGen

randomGens :: StdGen -> [StdGen]
randomGens g = let (a,b) = split g in a : randomGens b

randomGenome :: Rand Genome
randomGenome = replicateM 100 getRandom

{-# INLINE getRandom #-}
getRandom :: (Random a) => Rand a
getRandom = do (x,g) <- (random <$> get)
               g `seq` put g
               x `seq` return x

{-# SPECIALIZE getRandomR :: (Int,Int) -> Rand Int #-}
getRandomR :: (Random a) => (a,a) -> Rand a
getRandomR range = do (x,g) <- (randomR range <$> get)
                      g `seq` put g
                      x `seq` return x

{-# INLINE getStdGen #-}
getStdGen :: Rand StdGen
getStdGen = state split

mixGenome :: Double -> Genome -> Genome -> Rand Genome
mixGenome changeChance mother father =
      do coin <- getRandom
         mix (if coin
               then (mother,father)
               else (father,mother))
      where
         mix (d:ominant,_:recessive) = (d:) <$>
                 do x <- getRandom
                    mix (if (x < changeChance)
                           then (recessive,ominant)
                           else (ominant,recessive))
         mix (_,_) = return []

mutateGenome :: Double -> Genome -> Rand Genome
mutateGenome flipChance = mapM (\x ->
    (do c <- getRandom
        return $ if c > flipChance then x else not x))

addPos :: [[a]] -> [[(Offset,a)]]
addPos = zipWith (\x -> map (\(y,a)->((x,y),a))) [0..] . map (zip [0..])

generateRaceTrack :: Int -> Int -> Rand (U2 (Offset,Color))
generateRaceTrack l w = fromListU2 . addPos <$> replicateM l (replicateM w generateColor)

generateColor :: Rand Color
generateColor = getRandomR (0,15)

drawFromList :: [a] -> Rand a
drawFromList xs = (xs !!) <$> getRandomR (0, length xs - 1)

-- technically, Empty == Teleporter 0 0
data Cell = Wall | Teleporter Int Int | Trap Int Int deriving (Eq,Show)
emptyCell :: Cell
emptyCell = Teleporter 0 0

generateCells :: Rand [Cell]
generateCells = do te1 <- genCell Teleporter 4
                   te2 <- genCell Teleporter 4
                   tr1 <- liftA2 Trap (getRandomR (-1,1)) (getRandomR (-1,1))
                   tr2 <- liftA2 Trap (getRandomR (-1,1)) (getRandomR (-1,1))
                   shuffle $ [Wall,Wall,te1,te2,invertCell te1, invertCell te2, tr1,tr2]++replicate 8 emptyCell where
           genCell ctor r = untilM (/=ctor 0 0) $ liftA2 ctor (getRandomR (-r,r)) (getRandomR (-r,r))
           invertCell (Teleporter a b) = Teleporter (-a) (-b)
           invertCell _ = error "invertCell on a non-Teleporter"

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
