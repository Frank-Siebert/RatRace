import RatRace.RatRace

import RatRace.Util -- for square, remove, RatRace.RatRace import should be enough
import RatRace.Types
import System.Random
import RatRace.Controller
import Control.Monad.Trans.State
import Control.Comonad


main :: IO ()
main = runContest []

square :: U2 Char
square = fromListU2 ["abc","def","ghi"]

rt = evalState createRaceTrack (mkStdGen 100)

single = U2 (U [] (U "" 'X' "") [])
