module Types where

data Tape a = Tape [a] a [a]
    deriving Show

data Instr = Loop [Instr] -- [...]
           | ShiftL       -- <
           | ShiftR       -- >
           | Inc          -- +
           | Dec          -- -
           | Print        -- ,
           | Read         -- .
    deriving (Eq, Show)

type Prog = [Instr]
