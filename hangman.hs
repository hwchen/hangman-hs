--hangman game. No internet yet.

import Control.Monad
import Control.Monad.State
import Data.Char
import Data.List.Split

-- data types

-- guess is spot and letter, indexed starting at 0
-- gameState should include (Result, current uncovered, target word, # wrong)
-- Answer should include (Correct, # wrong, current uncovered)

-- Also need game ID to properly track multiple games?

type Guess = (Int, Char) 

data Answer = Correct | Wrong | Init deriving (Show)
data Result = Won | Lost | Continue deriving (Show)

data GameState = GameState { result :: Result
                           , currentG :: String
                           , target :: String
                           , countWrongG :: Int
                           } deriving (Show)

data Feedback = Feedback { answer :: Answer
                         , currentF :: String
                         , countWrongF :: Int
                         } deriving (Show)

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


guess :: Guess -> State GameState Feedback 
guess (spot, c) = do
    g <- get
    if target g !! spot == c 
        -- need another if to check for continue or Win
        then do 
            put GameState { result = Continue
                          , currentG =  (replaceC (spot, c) $ currentG g)
                          , target = target g
                          , countWrongG = countWrongG g
                          }
            return Feedback { answer = Correct
                            , currentF = (replaceC (spot, c) $ currentG g)
                            , countWrongF = countWrongG g
                            }
        else do 
        -- need to check if Continue or lost and put in a var for constructor
            put GameState { result = Continue
                          , currentG = currentG g
                          , target = target g
                          , countWrongG = (countWrongG g) + 1
                          }
            return Feedback { answer = Wrong
                            , currentF = currentG g
                            , countWrongF = (countWrongG g) + 1
                            }
            
initState :: String -> GameState
initState s = GameState { result = Continue
                        , currentG = (map (\x -> '*') s)
                        , target = s
                        , countWrongG = 0
                        }

testG = (guess (1,'n'))

-- test chain by using >>
g3 = do
    guess (2,'e')
    guess (1,'n')

main :: IO()
main = do
    --wordlist <- liftM cleanWordList $ wordList "words.txt"
    print $ runState (g3) $ initState "one" 
