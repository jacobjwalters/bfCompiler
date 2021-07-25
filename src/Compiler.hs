module Compiler where

import Control.Applicative ((<|>), some)
import Parser

data Tape a = Tape [a] a [a]
    deriving Show

data Instr = Loop [Instr] -- [...]
           | Back         -- <
           | Forward      -- >
           | Inc          -- +
           | Dec          -- -
           | Print        -- ,
           | Read         -- .
    deriving Show

type Prog = [Instr]

-- | Default tape instance for a given list
listToTape :: [a] -> Maybe (Tape a)
listToTape []     = Nothing
listToTape (x:xs) = Just $ Tape [] x xs

stripComments :: String -> String
stripComments = filter (`elem` "[]<>+-,.")

parseInstr :: Parser Instr
parseInstr = foldl1 (<|>)
    [ Back    <$ char '<'
    , Forward <$ char '>'
    , Inc     <$ char '+'
    , Dec     <$ char '-'
    , Print   <$ char ','
    , Read    <$ char '.'
    , Loop    <$>
        (char '[' *>
         some parseInstr
         <* char ']')]

parseProg :: Parser Prog
parseProg = some parseInstr <* eof

parse :: String -> Maybe Prog
parse str = snd <$> runParser parseProg str

