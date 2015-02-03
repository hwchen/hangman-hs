--hangman game. No internet yet.

import Control.Monad
import Control.Monad.State
import Data.Char
import Data.List.Split

-- data types
-- GameState should be a State Monad!

-- Load file

wordList :: FilePath -> IO [String]
wordList path = do
    content <- readFile path
    let res = splitOn "," content
    return res

cleanWordList :: [String] -> [String]
cleanWordList s = map (map toLower) $ map (filter (/= '\"')) s

-- initializze game

initGame :: State String String
initGame = do
    target <- get
    put target 
    let current = map (\c -> '*') target
    return current

-- first brush with State monad...
-- intermediate result should be whether guess is right or wrong!
-- and the modified state is the current state of the game.
-- the only other question is, what's the best way to pass in a 
-- guess? The "get" takes in a State. the "put" modifies thet state.
-- the "return" returns an intermediate value.
-- when you runState, you get back a tuple with fst is intermediate
-- result, and snd as the modified state.

guess :: String -> State String String
guess g = do
    current <- get
    return current

main :: IO()
main = do
    wordlist <- liftM cleanWordList $ wordList "words.txt"
    print $ wordlist
