module Optimiser
    ( optimise
    ) where

import Types

optimise :: Prog -> Prog
optimise [] = []
optimise (Loop []:is) = optimise is
optimise (Loop inner:outer) = Loop (optimise inner) : optimise outer
optimise (ShiftL:ShiftR:is) = optimise is
optimise (ShiftR:ShiftL:is) = optimise is
optimise (Inc:Dec:is) = optimise is
optimise (Dec:Inc:is) = optimise is
optimise (i:is) = let is' = optimise is
                  in if is == is'
                        then i:is
                        else optimise (i:is')
