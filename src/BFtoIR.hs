module BFtoIR
    ( generateIR
    , optimiseIR
    , initialArithsToSets
    )
    where

import Data.List (takeWhile, dropWhile, sortOn)

import Types

type Pass = IRProg -> IRProg

bfToIR :: Instr -> IR
bfToIR (Loop [Dec]) = Set 0 0
bfToIR (Loop [Inc]) = Set 0 0
bfToIR (Loop is) = ILoop $ map bfToIR is
bfToIR ShiftL = Shift (-1)
bfToIR ShiftR = Shift 1
bfToIR Inc = Add 1 0
bfToIR Dec = Add (-1) 0
bfToIR Read = GetC 0
bfToIR Print = PutC 0

collateAdds :: Pass
collateAdds [] = []
collateAdds is@(Add _ _:_) = Add (sum $ map getValue $ takeWhile isAdd is) 0
                                : collateAdds (dropWhile isAdd is)
collateAdds (ILoop is:is') = ILoop (collateAdds is) : collateAdds is'
collateAdds (i:is) = i:collateAdds is

collateShifts :: Pass
collateShifts [] = []
collateShifts is@(Shift _:_) = Shift (sum $ map getOffset $ takeWhile isShift is)
                                : collateShifts (dropWhile isShift is)
collateShifts (ILoop is:is') = ILoop (collateShifts is) : collateShifts is'
collateShifts (i:is) = i:collateShifts is

collateAriths :: Pass
collateAriths [] = []
collateAriths (Add _ _:Set x o:is) = collateAriths (Set x o:is)
collateAriths (Set a o:Add b p:is) = if o == p
                                         then Set (a+b) o    :collateAriths is
                                         else Set a o:Add b p:collateAriths is
collateAriths (i:is) = i:collateAriths is

shiftsToOffsets :: Pass
shiftsToOffsets [] = []
shiftsToOffsets (ILoop ls:is) = ILoop (shiftsToOffsets ls):shiftsToOffsets is
shiftsToOffsets (Shift o1:Add x o2:Shift o3:is) = Add x (o2+o1)
                                                : Shift (o1+o3)
                                                : shiftsToOffsets is
shiftsToOffsets (i:is) = i:shiftsToOffsets is

removeNops :: Pass
removeNops [] = []
removeNops (ILoop ls:is) = ILoop (removeNops ls) : removeNops is
removeNops (Add  0 x:is) = removeNops is
removeNops (Mult 1 x:is) = removeNops is
removeNops (Shift  0:is) = removeNops is
removeNops (i:is) = i:removeNops is

loopAddsToMults :: Pass
loopAddsToMults [] = []
loopAddsToMults (ILoop [Add (-1) o1, Add i o2]:is) = Set 0 o1
                                                   : Mult i o2
                                                   : loopAddsToMults is
loopAddsToMults (i:is) = i:loopAddsToMults is

collectOffsets :: Pass
collectOffsets [] = []
collectOffsets (ILoop ls:is) = ILoop (collectOffsets ls):collectOffsets is
collectOffsets is = sortOn getOffset is' ++ collectOffsets is''
    where (is',is'') = break isLoop is

-- Fix me - might have to pass along the current state for mult/add?
initialArithsToSets :: Pass
initialArithsToSets [] = []
initialArithsToSets i@(ILoop _:_) = i
initialArithsToSets i@(PutC  _:_) = i
initialArithsToSets i@(GetC  _:_) = i
initialArithsToSets i@(Shift _:_) = i
initialArithsToSets i@(Copy  _:_) = i
initialArithsToSets (Set  v o:is) = Set v o:initialArithsToSets is
initialArithsToSets (Add  v o:is) = Set v o:initialArithsToSets is
initialArithsToSets (Mult v o:is) = initialArithsToSets is


generateIR :: BFProg -> IRProg
generateIR = map bfToIR

optimiseIR :: Pass
optimiseIR = collateAriths . collectOffsets
           . loopAddsToMults . removeNops . shiftsToOffsets
           . collateShifts . collateAdds
