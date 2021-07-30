module Types where

data Tape a = Tape [a] a [a]
instance Show a => Show (Tape a) where
    show (Tape as b cs) = show [reverse as, [b], cs]

-- Different types as we want different bounds
type Offset = Int
type Value  = Int

type Mem = Tape Value

flipTape :: Tape a -> Tape a
flipTape (Tape as b cs) = Tape cs b as


data IR = Loop [IR]  -- Generally to be avoided
        | Set Value Offset
        | Add Value Offset
        | Mult Value Offset
        | Shift Offset
        | Copy Offset
        | Read Offset
        | Print Offset
    deriving (Eq, Show)

type Prog = [IR]


getValue :: IR -> Value
getValue (Set x _)  = x
getValue (Add x _)  = x
getValue (Mult x _) = x
getValue _ = error "Couldn't getValue for this type!"

getOffset :: IR -> Offset
getOffset (Set _ x)  = x
getOffset (Add _ x)  = x
getOffset (Mult _ x) = x
getOffset (Shift x)  = x
getOffset (Copy  x)  = x
getOffset (Read  x)  = x
getOffset (Print x)  = x
getOffset _ = error "Can't getOffset for loops"


isAdd :: IR -> Bool
isAdd (Add _ _) = True
isAdd _         = False

isShift :: IR -> Bool
isShift (Shift _) = True
isShift _         = False

isLoop :: IR -> Bool
isLoop (Loop _) = True
isLoop _        = False
