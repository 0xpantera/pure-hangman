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


main :: IO ()
main = do
  putStrLn "hello"


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
