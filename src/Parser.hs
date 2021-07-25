{-# LANGUAGE DeriveFunctor, LambdaCase #-}

module Parser where

import Control.Applicative ( Alternative (empty, (<|>)) )
import Control.Monad ((>=>))

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }
    deriving Functor

instance Applicative Parser where
    pure a = Parser $ \input -> Just (input, a)

    (Parser p1) <*> (Parser p2) =
        Parser $ p1 >=> \(input', f) ->
                p2 input' >>= \(input'', a) ->
                    Just (input'', f a)

instance Alternative Parser where
    empty = Parser $ const Nothing

    (Parser p1) <|> (Parser p2) =
        Parser $ \input -> p1 input <|> p2 input

char :: Char -> Parser Char
char c = Parser $ \case
    (x:xs) | x == c -> Just (xs, x)
    _               -> Nothing

eof :: Parser ()
eof = Parser $ \case
    [] -> Just mempty
    _  -> Nothing
