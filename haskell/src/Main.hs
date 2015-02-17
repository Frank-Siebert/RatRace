import RatRace.RatRace

import RatRace.Util -- for square, remove, RatRace.RatRace import should be enough
import RatRace.Types (U2)

main :: IO ()
main = runContest []

square :: U2 Char
square = fromListU2 ["ab","cd"]
