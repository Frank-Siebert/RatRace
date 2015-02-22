import RatRace.RatRace

import RatRace.Util -- for square, remove, RatRace.RatRace import should be enough
import RatRace.Types
import System.Random
import RatRace.Controller
import Control.Comonad


main :: IO ()
main = runContest [blind West, blind StandStill, blind East]

square :: U2 Char
square = fromListU2 ["abc","def","ghi"]

single = U2 (U [] (U "" 'X' "") [])

blind :: Move -> Player
blind x _ _ _ = x
