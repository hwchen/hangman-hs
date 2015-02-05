--hangman game. No internet yet.

import Control.Monad
import Control.Monad.State
import Data.Char
import Data.List.Split
import System.Random

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

-- should I create random generator outside of function and pass in?
getTarget :: [String] -> IO String
getTarget xs = do
    let limit = length xs
    g <- newStdGen
    return $ xs !! (fst $ (randomR (1,limit) g))


main :: IO()
main = do
    --initialise wordlist, target, gameState
    wordlist <- liftM cleanWordList $ wordList "words.txt"
    --session loop starts here. (multiple games inside)
    --get target (from random generator)
    tar <- getTarget wordlist
    print $ initState tar
    -- game loop starts here

    print $ "current game is: " ++ evalState (gets currentG) (initState tar)
    
    -- get guess: spot and letter
    print "Guess a spot"
    spot <- getLine 
    print "Guess a letter"
    letter <- getLine
    -- format with fmt
    print $ "(" ++ spot ++ ", " ++ letter ++ ")"
        --iterate game
    runState (guess (spot, letter))

    -- if state is Continue, then loop
    -- looks like I need to either runState to a new loop each time,
    -- or use Monad Transformers!!!

    --game over
    print $ runState (g3) $ initState "one" 
