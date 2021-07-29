module Types where

data Tape a = Tape [a] a [a]
instance Show a => Show (Tape a) where
    show (Tape as b cs) = show [reverse as, [b], cs]

type Mem = Tape Int

data Instr = Loop [Instr] -- [...]
           | ShiftL       -- <
           | ShiftR       -- >
           | Inc          -- +
           | Dec          -- -
           | Read         -- ,
           | Print        -- .
    deriving Eq

instance Show Instr where
    show (Loop as) = show $ map show as
    show ShiftL = "<"
    show ShiftR = ">"
    show Inc    = "+"
    show Dec    = "-"
    show Read   = ","
    show Print  = "."

type BFProg = [Instr]

-- Different types as we want different bounds
type Offset = Int
type Value  = Int

data IR = ILoop [IR]  -- Generally to be avoided
        | Set Value Offset
        | Add Value Offset
        | Mult Value Offset
        | Shift Offset
        | Copy Offset
        | GetC Offset
        | PutC Offset
    deriving (Eq, Show)

type IRProg = [IR]


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
getOffset (Copy x)   = x
getOffset (GetC x)   = x
getOffset (PutC x)   = x
getOffset _ = error "no offset for loops"


isAdd :: IR -> Bool
isAdd (Add _ _) = True
isAdd _         = False

isShift :: IR -> Bool
isShift (Shift _) = True
isShift _         = False

isLoop :: IR -> Bool
isLoop (ILoop _) = True
isLoop _ = False
