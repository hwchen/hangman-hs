--hangman game. No internet yet.

import Control.Monad
import Control.Monad.State
import Data.Char
import Data.List.Split

-- data types

-- guess is spot and letter, indexed starting at 0
type Guess = (Int, Char) 

data Answer = Correct | Wrong | Init deriving (Show)
data Result = Won | Lost | Continue deriving (Show)

-- Load file

wordList :: FilePath -> IO [String]
wordList path = do
    content <- readFile path
    let res = splitOn "," content
    return res

cleanWordList :: [String] -> [String]
cleanWordList s = map (map toLower) $ map (filter (/= '\"')) s

-- Needs a Maybe in case the guess is out of bounds. Right now,
-- just appends guess to end of string
replaceC :: Guess -> String -> String 
replaceC (spot, c) s = take spot s ++ [c] ++ drop (spot+1) s

-- initializze game (this might be extraneous, but works for now)

initGame :: State (Result, String, String) Answer 
initGame = do
    target <- get
    let (_,_,t) = target
    let current = map (\c -> '*') t
    put (Continue, current, t)
    return Init

-- first brush with State monad...
-- intermediate result should be whether guess is right or wrong!
-- and the modified state is the current state of the game.
-- the only other question is, what's the best way to pass in a 
-- guess? The "get" takes in a State. the "put" modifies thet state.
-- the "return" returns an intermediate value.
-- when you runState, you get back a tuple with fst is intermediate
-- result, and snd as the modified state.

-- Doesn't check result here, that's done previously in the gameloop.
-- also, no error check
-- what is >>= for in State monad?

guess :: Guess -> State (Result, String, String) Answer 
guess (spot, c) = do
    t <- get
    let (_ , current, target) = t
    if target !! spot == c 
        -- need another if to check for continue or Win
        then do 
            put (Continue, (replaceC (spot, c) current), target)
            return Correct
        else do 
            put (Continue, current, target)
            return Wrong
            
initS = (Continue, "***", "one")
testG = (guess (1,'n'))

main :: IO()
main = do
    wordlist <- liftM cleanWordList $ wordList "words.txt"
    print $ wordlist
    print $ runState (guess (1, 'g')) (Continue, "***", "one")
    print $ runState testG $ execState initGame initS
    --runState initGame (Continue, "xxx", "one") >>= runState (guess (1,'g')) >>= print
    -- chain works! but is there a better way to chain the calculations together?
    -- it seems like it would be nice to do it with an operator, instead of execState
