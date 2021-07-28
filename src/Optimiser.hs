module Optimiser
    ( cancelDups
    , reorderShifts
    , rmDeadLoops
    , rmDeadShifts
    , optimise
    ) where

import Data.List

import Types

type Pass = Prog -> Prog

-- | Removes loops at the start of the program, as all cells are zero (so they'd
-- be skipped regardless).
rmDeadLoopsStart :: Pass
rmDeadLoopsStart []          = []
rmDeadLoopsStart (Loop _:is) = rmDeadLoopsStart is
rmDeadLoopsStart is          = is

-- | If we have a loop followed immediately by another loop, the second won't
-- run, since we've just set the cell to 0. Remove the second loop.
rmDeadLoopsBody :: Pass
rmDeadLoopsBody []                  = []
rmDeadLoopsBody (Loop ls:Loop _:is) = Loop ls : rmDeadLoopsBody is
rmDeadLoopsBody (i:is)              = i : rmDeadLoopsBody is

rmDeadLoops :: Pass
rmDeadLoops = rmDeadLoopsBody . rmDeadLoopsStart

-- | Shifts at the end of a program have no effect.
rmDeadShifts :: Pass
rmDeadShifts = reverse . dropWhile (isShift) . reverse
    where isShift ShiftL = True
          isShift ShiftR = True
          isShift _      = False

-- | Cancels out adjacent `[]` and `+-`s
cancelDups :: Pass
cancelDups [] = []
cancelDups (Loop []:is)       = cancelDups is
cancelDups (Loop inner:outer) = Loop (cancelDups inner) : cancelDups outer
cancelDups (ShiftL:ShiftR:is) = cancelDups is
cancelDups (ShiftR:ShiftL:is) = cancelDups is
cancelDups (Inc:Dec:is)       = cancelDups is
cancelDups (Dec:Inc:is)       = cancelDups is
cancelDups (i:is) = let is'   = cancelDups is
                  in if is == is'
                        then i:is
                        else cancelDups (i:is')

-- | Reorders e.g. `+>+<+` to `++>+<`
-- Useful for later compilation phases
reorderShifts :: Pass
reorderShifts [] = []
reorderShifts is = reorder is' ++ reorderShifts is''
    where rearrangeable Inc    = True
          rearrangeable Dec    = True
          rearrangeable ShiftL = True
          rearrangeable ShiftR = True
          rearrangeable _      = False
          is'  = takeWhile (rearrangeable) is
          is'' = dropWhile (rearrangeable) is
          reorder :: Pass
          reorder = id

optimise :: Pass
optimise = cancelDups . reorderShifts . rmDeadLoops
