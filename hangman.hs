--hangman game. No internet yet.

import Control.Monad
import Control.Monad.State
import Data.Char
import Data.List.Split
import Data.String.Utils
import System.Random

-- data types

-- guess is spot and letter, indexed starting at 0
-- gameState should include (Result, current uncovered, target word, # wrong)
-- Answer should include (Correct, # wrong, current uncovered)

-- Also need game ID to properly track multiple games?

type Guess = (Int, Char) 

data Answer = Correct | Wrong | Init deriving (Show, Eq)
data Result = Won | Lost | Continue deriving (Show, Eq)

data GameState = GameState { result :: Result
                           , currentG :: String
                           , target :: String
                           , countWrongG :: Int
                           } deriving (Show, Eq)

data Feedback = Feedback { answer :: Answer
                         , currentF :: String
                         , countWrongF :: Int
                         } deriving (Show, Eq)

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

drawHangman :: Int -> String
drawHangman 10 =   "________\n"
                ++ "|      |\n"
                ++ "|      |\n"
                ++ "|      O\n"
                ++ "|    --|--\n"
                ++ "|     / \\ \n"
                ++ "| \n"
                ++ "|_________\n"
drawHangman 9 =    "________\n"
                ++ "|      |\n"
                ++ "|      |\n"
                ++ "|      O\n"
                ++ "|    --|--\n"
                ++ "|     / \n"
                ++ "| \n"
                ++ "|_________\n"
drawHangman 8 =    "________\n"
                ++ "|      |\n"
                ++ "|      |\n"
                ++ "|      O\n"
                ++ "|    --|--\n"
                ++ "|     \n"
                ++ "| \n"
                ++ "|_________\n"
drawHangman 7 =    "________\n"
                ++ "|      |\n"
                ++ "|      |\n"
                ++ "|      O\n"
                ++ "|    --|\n"
                ++ "|     \n"
                ++ "| \n"
                ++ "|_________\n"
drawHangman 6 =    "________\n"
                ++ "|      |\n"
                ++ "|      |\n"
                ++ "|      O\n"
                ++ "|      |\n"
                ++ "|     \n"
                ++ "| \n"
                ++ "|_________\n"
drawHangman 5 =    "________\n"
                ++ "|      |\n"
                ++ "|      |\n"
                ++ "|      O\n"
                ++ "|    \n"
                ++ "|     \n"
                ++ "| \n"
                ++ "|_________\n"
drawHangman 4 =    "________\n"
                ++ "|      |\n"
                ++ "|      |\n"
                ++ "|      \n"
                ++ "|    \n"
                ++ "|     \n"
                ++ "| \n"
                ++ "|_________\n"
drawHangman 3 =    "________\n"
                ++ "|      \n"
                ++ "|      \n"
                ++ "|      \n"
                ++ "|    \n"
                ++ "|     \n"
                ++ "| \n"
                ++ "|_________\n"
drawHangman 2 =    "|      \n"
                ++ "|      \n"
                ++ "|      \n"
                ++ "|    \n"
                ++ "|    \n"
                ++ "| \n"
                ++ "|_________\n"
drawHangman 1 = "|_________\n"
drawHangman 0 = "None Wrong!"


guess :: Guess -> State GameState Feedback 
guess (spot, c) = do
    g <- get
    if target g !! spot == c 
        -- if right answer
        then do
            let output_result = if target g == (replaceC (spot, c) $ currentG g)
                    then Won
                    else Continue
            let output_current = (replaceC (spot, c) $ currentG g)
                output_target = target g
                output_wrong = countWrongG g
                output_answer = Correct

            put GameState { result = output_result 
                            , currentG = output_current 
                            , target = output_target 
                            , countWrongG = output_wrong 
                            }
            return Feedback { answer = output_answer 
                            , currentF = output_current 
                            , countWrongF = output_wrong
                            }
        -- if wrong answer
        else do
            -- haha, funny imperative if statement
            --if countWrongG g == 9 
                --then output_result = Lose
                --else output_result = Continue
            let output_result = if countWrongG g == 9
                    then Lost 
                    else Continue
            let output_current = currentG g 
                output_target = target g
                output_wrong = (countWrongG g) + 1
                output_answer = Wrong

            put GameState { result = output_result 
                            , currentG = output_current 
                            , target = output_target 
                            , countWrongG = output_wrong 
                            }
            return Feedback { answer = output_answer 
                            , currentF = output_current
                            , countWrongF = output_wrong 
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

gameLoop :: GameState -> IO()
gameLoop state = do
    print $ "current game is: " ++ evalState (gets currentG) state 
    -- get guess: spot and letter
    -- need to fix to make it less hacky
    print "Guess a spot"
    spot <- fmap digitToInt getChar 
    print "Guess a letter"
    letter <- getChar
    -- format with fmt
    print $ "(" ++ (show spot) ++ ", " ++ [letter] ++ ")"
    --iterate game
    let (feedback, newRound) = runState (guess (spot, letter)) state
    print feedback 
    putStrLn $ drawHangman $ countWrongF feedback
    if result newRound == Continue
        then gameLoop newRound
        else return ()

main :: IO()
main = do
    --initialise wordlist, target, gameState
    wordlist <- liftM cleanWordList $ wordList "words.txt"
    --session loop starts here. (multiple games inside)
    tar <- getTarget wordlist
    print $ initState tar
    -- game loop starts here
    gameLoop $ initState tar
    print $ "Game Over"
    -- looks like I need to either runState to a new loop each time,
    -- or use Monad Transformers!!!
    -- next step is to validate input
    -- then clean some code...
