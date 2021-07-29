module Compiler where

import Control.Applicative  ((<|>), some)
import Data.Char
import Data.Maybe           ( fromJust )

import Types
import Parser
import BFOptimiser
import BFtoIR

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
    , Read   <$ char ','
    , Print  <$ char '.'
    , Loop   <$>
        (char '[' *>
         some parseInstr
         <* char ']')]

parseProg :: Parser BFProg
parseProg = some parseInstr <* eof

parse :: String -> Maybe BFProg
parse str = snd <$> runParser parseProg str


-- INTERPRETER STUFF
shiftL, shiftR :: Tape a -> Tape a
shiftL t@(Tape []     b cs)     = t
shiftL   (Tape (a:as) b cs)     = Tape as a (b:cs)
shiftR t@(Tape as     b [])     = t
shiftR   (Tape as     b (c:cs)) = Tape (b:as) c cs

alterCell :: (a -> a) -> Tape a -> Tape a
alterCell f (Tape as b cs) = Tape as (f b) cs

getCell :: Tape a -> a
getCell (Tape _ a _) = a

setCell :: a -> Tape a -> Tape a
setCell = alterCell . const

-- | Interprets a program in a given memory state
runBF :: Mem -> BFProg -> IO Mem
runBF mem [] = return mem
runBF mem (i:is) = do
    case i of
         ShiftL  -> runBF (shiftL            mem) is
         ShiftR  -> runBF (shiftR            mem) is
         Inc     -> runBF (alterCell (+1)    mem) is
         Dec     -> runBF (alterCell (+(-1)) mem) is
         Read    -> do
             c <- getChar
             runBF (setCell (ord c) mem) is
         Print   -> do
             putChar $ chr $ getCell mem
             runBF mem is
         Loop is' -> case getCell mem of
                         0 -> runBF mem is
                         _ -> runBF mem is' >>= flip runBF (i:is)



emptyMem :: Mem
emptyMem = fromJust $ listToTape $ replicate 256 0

helloworld :: String
helloworld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.<<"

hw :: BFProg
hw = fromJust $ parse helloworld

parseIR :: String -> IRProg
parseIR = generateIR . fromJust . parse
