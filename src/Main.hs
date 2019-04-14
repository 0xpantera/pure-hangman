module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)


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
  where
    gameLength w =
      let l = length (w :: String)
      in l >= minWordLength
         && l < maxWordLength


randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex


randomWord' :: IO String
randomWord' = gameWords >>= randomWord


data Puzzle = Puzzle String [Maybe Char] [Char]


instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
     fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

    
freshPuzzle :: String -> Puzzle
freshPuzzle wordAtPlay = Puzzle wordAtPlay discovered []
  where
    discovered = map (const Nothing) wordAtPlay


charInWord :: Char -> Puzzle -> Bool
charInWord guess (Puzzle wordAtPlay _ _) = guess `elem` wordAtPlay


alreadyGuessed :: Char -> Puzzle -> Bool
alreadyGuessed guess (Puzzle _ _ guessed) = guess `elem` guessed


renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c


fillInCharacter :: Char -> Puzzle -> Puzzle
fillInCharacter c (Puzzle word filledIn s) =
  Puzzle word newFilledIn (c : s)
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
      then Just wordChar
      else guessChar
    newFilledIn =
      zipWith (zipper c) word filledIn


handleGuess :: Char -> Puzzle -> IO Puzzle
handleGuess guess puzzle = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord guess puzzle, alreadyGuessed guess puzzle) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else."
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word."
      return (fillInCharacter guess puzzle)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter guess puzzle)


gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 7
  then do putStrLn "You lose"
          putStrLn $ "The word was: " ++ wordToGuess
          exitSuccess
  else return ()


gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar
  then do putStrLn "You win"
          exitSuccess
  else return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess c puzzle >>= runGame
    _   -> putStrLn "Your guess must be a single character"


main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
