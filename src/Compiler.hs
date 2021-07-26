module Compiler where

import Control.Applicative ((<|>), some)

import Types
import Parser
import Optimiser

-- | Default tape instance for a given list
listToTape :: [a] -> Maybe (Tape a)
listToTape []     = Nothing
listToTape (x:xs) = Just $ Tape [] x xs

stripComments :: String -> String
stripComments = filter (`elem` "[]<>+-,.")

parseInstr :: Parser Instr
parseInstr = foldl1 (<|>)
    [ ShiftL <$ char '<'
    , ShiftR <$ char '>'
    , Inc    <$ char '+'
    , Dec    <$ char '-'
    , Print  <$ char ','
    , Read   <$ char '.'
    , Loop   <$>
        (char '[' *>
         some parseInstr
         <* char ']')]

parseProg :: Parser Prog
parseProg = some parseInstr <* eof

parse :: String -> Maybe Prog
parse str = snd <$> runParser parseProg str

