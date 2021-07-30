module Optimiser
    ( optimise
    , initialArithsToSets
    )
    where

import Data.List (takeWhile, dropWhile, sortOn)

import Types

type Pass = Prog -> Prog

collateAdds :: Pass
collateAdds [] = []
collateAdds is@(Add _ _:_) = Add (sum $ map getValue $ takeWhile isAdd is) 0
                                : collateAdds (dropWhile isAdd is)
collateAdds (Loop is:is') = Loop (collateAdds is) : collateAdds is'
collateAdds (i:is) = i:collateAdds is

collateShifts :: Pass
collateShifts [] = []
collateShifts is@(Shift _:_) = Shift (sum $ map getOffset $ takeWhile isShift is)
                             : collateShifts (dropWhile isShift is)
collateShifts (Loop is:is')  = Loop (collateShifts is) : collateShifts is'
collateShifts (i:is) = i:collateShifts is

collateAriths :: Pass
collateAriths [] = []
collateAriths (Add _ _:Set x o:is) = collateAriths (Set x o:is)
collateAriths (Set a o:Add b p:is) = if o == p
                                         then Set (a+b) o    :collateAriths is
                                         else Set a o:Add b p:collateAriths is
collateAriths (i:is) = i:collateAriths is

-- | Converts shift-add-shift forms to @Add@s with an offset
shiftsToOffsets :: Pass
shiftsToOffsets [] = []
shiftsToOffsets (Loop ls:is) = Loop (shiftsToOffsets ls):shiftsToOffsets is
shiftsToOffsets (Shift o1:Add x o2:Shift o3:is) = Add x (o2+o1)
                                                : Shift (o1+o3)
                                                : shiftsToOffsets is
shiftsToOffsets (i:is) = i:shiftsToOffsets is


---------- DEAD CODE REMOVAL
-- | Removes any operations that do nothing.
rmNops :: Pass
rmNops [] = []
rmNops (Loop []:is)  = rmNops is
rmNops (Loop ls:is)  = Loop (rmNops ls) : rmNops is
rmNops (Add  0 x:is) = rmNops is
rmNops (Mult 1 x:is) = rmNops is
rmNops (Shift  0:is) = rmNops is
rmNops (i:is) = i:rmNops is

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
rmDeadLoopsBody (Loop ls:Loop _:is) = rmDeadLoopsBody (Loop ls : is)
rmDeadLoopsBody (i:is)              = i : rmDeadLoopsBody is

rmDeadLoops :: Pass
rmDeadLoops = rmDeadLoopsBody . rmDeadLoopsStart

-- | Shifts at the end of a program have no effect.
rmDeadShifts :: Pass
rmDeadShifts = reverse . dropWhile isShift . reverse


---------- LOOPS TO DEDICATED OPS
-- | Convert code of the form `[->+..+<]` to multiply operations
loopAddsToMults :: Pass
loopAddsToMults [] = []
loopAddsToMults (Loop [Add (-1) o1, Add i o2]:is) = Set 0 o1
                                                  : Mult i o2
                                                  : loopAddsToMults is
loopAddsToMults (i:is) = i:loopAddsToMults is

-- | Group instructions by their offset. Reduces the number of shifts needed.
collectOffsets :: Pass
collectOffsets []           = []
collectOffsets (Loop ls:is) = Loop (collectOffsets ls):collectOffsets is
collectOffsets is           = sortOn getOffset is' ++ collectOffsets is''
    where (is',is'') = break isLoop is

-- Fix me - might have to pass along the current state for mult/add?
initialArithsToSets :: Pass
initialArithsToSets [] = []
initialArithsToSets i@(Loop  _:_) = i
initialArithsToSets i@(Print _:_) = i
initialArithsToSets i@(Read  _:_) = i
initialArithsToSets i@(Shift _:_) = i
initialArithsToSets i@(Copy  _:_) = i
initialArithsToSets (Set  v o:is) = Set v o:initialArithsToSets is
initialArithsToSets (Add  v o:is) = Set v o:initialArithsToSets is
initialArithsToSets (Mult v o:is) = initialArithsToSets is

optimise :: Pass
optimise = collateAriths -- . collectOffsets
           . loopAddsToMults . rmNops . shiftsToOffsets
           . collateShifts . collateAdds . rmDeadLoops . rmDeadShifts
