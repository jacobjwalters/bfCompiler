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
           | Print        -- ,
           | Read         -- .
    deriving (Eq, Show)

type Prog = [Instr]
