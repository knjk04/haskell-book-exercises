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

-- main :: IO ()
-- main = do
--   putStrLn "hello world"

-- type WordList = [String]

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "../data/dict.txt"
  -- return (lines dict)
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          -- in     l >= minWordLength
          in     l > minWordLength
          && l <  maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <-
    randomRIO (0, (length wl) - 1 )
  return $ wl !! randomIndex
  -- randomIndex <- randomRIO (0, (length wl) - 1 )
  -- return $ wl !! randomIndex

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

-- insert a correctly guessed character into the string
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar

        newFilledInSoFar =
          zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

-- stop after a certain number of guesses (correct or otherwise)
gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 7 then
    do putStrLn "You lose!"
       putStrLn $
         "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

-- since the puzzle is a list of Maybe values, when every character is represented by
-- a Just Char instead of a Nothing, you win the game and exit
gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

-- forever makes the instructions run indefinitely 
runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   ->
      putStrLn "Your guess must\
              \ be a single character"

-- gets a word from a list of generated words, generates a new puzzle and executes runGame
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle =
        freshPuzzle (fmap toLower word)
  runGame puzzle

