module Compiler where

import Control.Applicative  ((<|>), many)
import Data.Char
import Data.Maybe           ( fromJust )

import Types
import Parser
import Optimiser

-- | Default tape instance for a given list
listToTape :: [a] -> Maybe (Tape a)
listToTape []     = Nothing
listToTape (x:xs) = Just $ Tape [] x xs

stripComments :: String -> String
stripComments = filter (`elem` "[]<>+-,.")

parseInstr :: Parser IR
parseInstr = foldl1 (<|>)
    [ Shift (-1) <$ char '<'
    , Shift 1 <$ char '>'
    , Add 1 0    <$ char '+'
    , Add (-1) 0    <$ char '-'
    , Read 0   <$ char ','
    , Print 0  <$ char '.'
    , Loop   <$>
        (char '[' *>
         many parseInstr
         <* char ']')]

parseProg :: Parser Prog
parseProg = many parseInstr <* eof

parse :: String -> Maybe Prog
parse str = snd <$> runParser parseProg str


-- INTERPRETER STUFF
shiftTape :: Offset -> Tape a -> Tape a
shiftTape o (Tape as b cs)
    | o <  0 = flipTape $ shift (-o) cs b as
    | o == 0 = Tape as b cs
    | o >  0 = shift o as b cs
    where shift o as b cs = Tape (tail (reverse $ take o cs) ++ [b] ++ as)
                                 (last $ take o cs)
                                 (drop o cs)
shiftTape o t = error $ "Invalid shiftTape input: " ++ show o

alterCell :: (a -> a) -> Offset -> Tape a -> Tape a
alterCell f o (Tape as b cs)
    | o <  0 = Tape (adjust f (abs o) as) b cs
    | o == 0 = Tape as (f b) cs
    | o >  0 = Tape as b (adjust f o cs)
    where adjust :: (a -> a) -> Offset -> [a] -> [a]
          adjust f o = uncurry (++) . second (\(x:xs) -> f x : xs) . splitAt o
          second f (a,b) = (a,f b)
alterCell _ o t = error $ "Invalid alterCell input" ++ show o

getCell :: Offset -> Tape a -> a
getCell = (getCell' .) . shiftTape
    where getCell' (Tape _ a _) = a

setCell :: a -> Offset -> Tape a -> Tape a
setCell = alterCell . const

-- | Interprets a program in a given memory state
runIR :: Mem -> Prog -> IO Mem
runIR mem [] = return mem
runIR mem (i:is) = do
    case i of
         Shift  o -> runIR (shiftTape      o mem) is
         Set  x o -> runIR (setCell     x  o mem) is
         Add  x o -> runIR (alterCell (+x) o mem) is
         Mult x o -> runIR (alterCell (*x) o mem) is
         Copy o   -> let cp = setCell (getCell 0 mem)
                     in runIR (cp o (cp (o+1) mem)) is
         Read   o -> do
             c <- getChar
             runIR (setCell (ord c) o mem) is
         Print  o -> do
             putChar $ chr $ getCell o mem
             runIR mem is
         Loop is' -> case getCell 0 mem of
                         0 -> runIR mem is
                         _ -> runIR mem is' >>= flip runIR (i:is)


emptyMem :: Mem
emptyMem = fromJust $ listToTape $ replicate 256 0

helloworld :: String
helloworld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.<<"

hw :: Prog
hw = fromJust $ parse helloworld

parseIR :: String -> Prog
parseIR = fromJust . parse
