{-# LANGUAGE Safe          #-}
{-# LANGUAGE UnicodeSyntax #-}
module M3U.Read (fromFile, fromString) where

import           M3U.Common

import           Data.Maybe                               (Maybe (Just))
import           Prelude.Unicode
import           System.FilePath.Posix                    (FilePath)

-- Parsec functions
import           Text.Parsec                              (parse)
import           Text.ParserCombinators.Parsec            (ParseError)
import           Text.ParserCombinators.Parsec.Char
import           Text.ParserCombinators.Parsec.Combinator
import           Text.ParserCombinators.Parsec.Prim       (Parser, many)

-- http://stackoverflow.com/questions/6270324/in-haskell-how-do-you-trim-whitespace-from-the-beginning-and-end-of-a-string
strip :: String -> String
strip = lstrip . rstrip
lstrip :: String -> String
lstrip = dropWhile (`elem` " \t")
rstrip :: String -> String
rstrip = reverse . lstrip . reverse

m3uParser ∷ Parser M3U
m3uParser = do firstLine
               entries ← many entryParser
               return $ M3U entries
    where firstLine = spaces >> string "#EXTM3U" >> newline

entryParser ∷ Parser M3UEntry
entryParser = do spaces
                 -- optional extinf line, otherwise, return nothing
                 extinf ← option Nothing extinfParser
                 spaces -- TODO: why won't newline work here?
                 path ← stringUntil '\n'
                 spaces -- TODO: why won't newline work here?
                 return $ M3UEntry extinf path

-- always returns the Just, Maybe is for "option"
extinfParser ∷ Parser (Maybe EXTINF)
extinfParser = do (spaces >> string "#EXTINF:" >> spaces)
                  secsChars ← many digit
                  let secs = read secsChars ∷ Int
                  char ','
                  spaces
                  -- TODO trim
                  artist ← rstrip <$> stringUntil '-' -- bandname: anything except -
                  char '-'
                  spaces
                  track ← stringUntil '\n' -- track name: anything except \n
                  newline
                  return $ Just (EXTINF secs artist track)

-- parse and return up to a certain character
stringUntil ∷ Char → Parser String
stringUntil c = many1 (noneOf [c])

-- |The 'fromString' function reads in a string, either returning an M3U or one
-- of Parsec's parse errors
fromString ∷ String → Either ParseError M3U
fromString = parse m3uParser "M3U parser"

-- |The 'fromFile' function reads in a file, possibly returning an M3U record
fromFile ∷ FilePath → IO (Either ParseError M3U)
fromFile path = (return ∘ fromString) =<< readFile path

