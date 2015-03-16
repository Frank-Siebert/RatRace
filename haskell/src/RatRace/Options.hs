module RatRace.Options where

import System.Console.GetOpt

data SimulationOptions = SimulationOptions {
    raceTrackLength    :: Int,
    raceTrackWidth     :: Int,
    rounds             :: Int,
    initialRatCount    :: Int,
    genomeLength       :: Int,
    genomeChangeChance :: Double,
    genomeFlipChance   :: Double,
    gameTurns          :: Int,
    initialScore       :: Int
} deriving (Show)

defaultOptions = SimulationOptions {
    raceTrackLength    = 54,
    raceTrackWidth     = 15,
    rounds             = 20,
    initialRatCount    = 15,
    genomeLength       = 100,
    genomeChangeChance = 0.05,
    genomeFlipChance   = 0.01,
    gameTurns          = 10000,
    initialScore       = 1
}

options :: [OptDescr (SimulationOptions -> SimulationOptions)]
options =
    [ Option ['g','r']     ["games","rounds"]
        (ReqArg (\x opts -> opts { rounds = read x }) "n")
        ("rounds (default: "++show (rounds defaultOptions)++")")
    , Option ['t'] ["turns"]
        (ReqArg (\x opts -> opts { gameTurns = read x }) "n")
        ("game turns (default: "++show (gameTurns defaultOptions)++")")
    , Option ['f'] ["flipchance","prob_mutation"]
        (ReqArg (\x opts -> opts { genomeFlipChance = read x }) "x")
        ("mutation probability (default: "++show (genomeFlipChance defaultOptions)++")")
    , Option [] ["xover","prob_crossover"]
        (ReqArg (\x opts -> opts { genomeChangeChance = read x }) "x")
        ("mutation crossover (default: "++show (genomeChangeChance defaultOptions)++")")
    ]

compilerOpts :: [String] -> SimulationOptions
compilerOpts argv =
    case getOpt Permute options argv of
         (o,_,[]  ) -> foldl (flip id) defaultOptions o
         (_,_,errs) -> error (concat errs ++ usageInfo "RatRace command line options" options)
