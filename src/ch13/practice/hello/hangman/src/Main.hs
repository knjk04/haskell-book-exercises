module Main where

import Control.Monad (forever) 
import Data.Char (toLower) 
import Data.Maybe (isJust) 
import Data.List (intersperse) 
import System.Exit (exitSuccess) 
import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout) 
import System.Random (randomRIO) 

main :: IO ()
main = do
  putStrLn "hello world"

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in     l >= minWordLength
          && l <  maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, (length wl) - 1 )
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

-- 1st argument: word we are trying to guess
-- 2nd argument: the characters that we have filled in so far
-- 3rd argument: the letters we've guessed so far
data Puzzle =
  Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
     fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle wordInPlay = Puzzle wordInPlay (map (const Nothing) wordInPlay) []

-- Example usage: charInWord (Puzzle "apple" [Just 'c'] []) 'a'
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle wordToGuess _ _) c = elem c wordToGuess

-- Example usage: 
-- *Main> p = Puzzle "apple" [Just 'c'] ['t']
-- *Main> alreadyGuessed p 't'
alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c
