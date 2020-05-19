import Data.Char (toUpper, toLower)
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad (when)
import System.IO 
import System.Exit
import HangmanUtils

data GameDifficulty = Easy | Medium | Hard deriving (Show, Eq)
data GameState = Running | Finished deriving (Show, Eq)

type WordToGuess = [Char];
type WordToPrint = [Char];
type FailedGuesses = [Char];
type GuessChar = Char;

printGraphic :: GameDifficulty -> WordToPrint -> FailedGuesses -> IO ()
printGraphic gameDifficulty wordToPrint failedGuesses =
                do
                graphic <- readFile $ "difficulty/" ++ (show gameDifficulty) ++ "/" ++ (show gameDifficulty) ++ (show $ length failedGuesses) ++ ".txt"
                putStrLn "" -- padding
                putStrLn graphic
                putStrLn "" -- padding
                putStrLn wordToPrint
                putStrLn "" -- padding
                case gameDifficulty of
                    Easy -> putStrLn $ "Failed guesses: " ++ failedGuesses
                    Medium -> putStrLn $ "Failed guesses: " ++ failedGuesses
                    Hard -> putChar ' '
                putStrLn "" -- padding

getDifficulty :: IO (Bool, GameDifficulty)
getDifficulty = do
                lastDifficulty <- readFile "difficulty/lastDifficulty.txt";
                case (map toLower lastDifficulty) of
                    "easy" -> return (True, Easy)
                    "medium" -> return (True, Medium)
                    "hard" -> return (True, Hard)
                    _ -> return (False, Medium)

_getNewWordToPrint :: Int -> WordToGuess -> WordToPrint -> GuessChar -> WordToPrint
_getNewWordToPrint index wordToGuess wordToPrint guessChar
    | index >= (length wordToGuess) = wordToPrint
    | (wordToGuess !! index == guessChar) = _getNewWordToPrint (index + 1) wordToGuess (replaceCharAtIndex (index*2) guessChar wordToPrint) guessChar
    | otherwise = _getNewWordToPrint (index + 1) wordToGuess wordToPrint guessChar
 
getNewWordToPrint  :: WordToGuess -> WordToPrint -> GuessChar -> WordToPrint
getNewWordToPrint = _getNewWordToPrint 0
      
getNewFailedGuesses :: WordToGuess -> GuessChar -> FailedGuesses -> FailedGuesses
getNewFailedGuesses wordToGuess guessChar failedGuesses
    | not (elem guessChar wordToGuess) && not (elem guessChar failedGuesses) = failedGuesses ++ (guessChar:[])
    | otherwise                                                              = failedGuesses

isGameLost :: GameDifficulty -> FailedGuesses -> Bool
isGameLost gameDifficulty failedGuesses
    | gameDifficulty == Easy   && (length failedGuesses) >= 12  = True
    | gameDifficulty == Medium && (length failedGuesses) >= 7   = True
    | gameDifficulty == Hard   && (length failedGuesses) >= 4   = True
    | otherwise                                                 = False

playGame :: GameDifficulty -> GameState -> WordToGuess -> WordToPrint -> FailedGuesses -> IO ()
playGame gameDifficulty gameState wordToGuess wordToPrint failedGuesses
    | gameState == Running =
        case isGameLost gameDifficulty failedGuesses of
            False -> 
                    do
                    printGraphic gameDifficulty wordToPrint failedGuesses
                    guessChar <- myGetChar
                    -- putChar guessChar
                    let newWordToPrint = getNewWordToPrint wordToGuess wordToPrint guessChar
                    let newFailedGuesses = getNewFailedGuesses wordToGuess guessChar failedGuesses
                    if areEqual wordToGuess newWordToPrint then
                        playGame gameDifficulty Finished wordToGuess newWordToPrint newFailedGuesses
                    else 
                        playGame gameDifficulty Running wordToGuess newWordToPrint newFailedGuesses
            True -> 
                    do
                    printGraphic gameDifficulty wordToPrint failedGuesses
                    putStrLn $ "YOU LOST!!! The word to guess was \"" ++ wordToGuess ++ "\"."
    | gameState == Finished =
                                do
                                printGraphic gameDifficulty wordToPrint failedGuesses
                                putStrLn "YOU WON!!!"

printHelp :: IO ()
printHelp = 
            do
            putStrLn ""
            putStrLn "Available commands:"
            putStrLn "play"
            putStrLn "diff {difficulty}"
            putStrLn "help"
            putStrLn "quit"
            putStrLn ""

-- available commands:
-- play
-- diff {difficulty}
-- help (show this menu)
-- quit
showCommandList :: IO ()
showCommandList = 
                do
                myPutStr "Enter command: "
                command <- getLine
                if (map toLower command) == "play" then -- play was entered
                    return ()
                else if (splitBy ' ' (map toLower command)) !! 0 == "diff" then -- difficulty <difficulty> was entered
                    do
                    let difficultyFile = "difficulty/lastDifficulty.txt"
                    let difficulty = ((splitBy ' ' command) !! 1)
                    case (map toLower difficulty) of
                        "easy"   -> do
                                    putStrLn $ "Changed difficulty to easy, maximum of 12 mistakes."
                                    writeFile difficultyFile difficulty
                        "medium" -> do 
                                    putStrLn $ "Changed difficulty to medium, maximum of 7 mistakes."
                                    writeFile difficultyFile difficulty
                        "hard"   -> do
                                    putStrLn $ "Changed difficulty to hard, maximum of 4 mistakes."
                                    writeFile difficultyFile difficulty
                        _ -> putStrLn $ "Unknown difficulty, choose one from the following set {Easy, Medium, Hard}"
                    showCommandList
                else if (map toLower command) == "help" then -- help was entered
                    do
                    printHelp
                    showCommandList
                else if (map toLower command) == "quit" then -- quit was entered
                    die ""
                else -- command not recognized
                    do
                    putStrLn "Command not recognized" 
                    showCommandList

main :: IO ()
main =
        do
        showCommandList
        difficultyConfig <- getDifficulty
        let hasBeenModified = fst difficultyConfig
        let lastDifficulty = snd difficultyConfig
        if hasBeenModified == False then
            putStrLn "Last difficulty could not be loaded, defaulting to medium difficulty."
        else
            putStrLn $ "Difficulty: " ++ (show lastDifficulty)
        myPutStr "Enter word to guess: "
        wordToGuess <- getLine
        let wordToPrint = foldr (\x acc -> "_ " ++ acc) [] wordToGuess
        let failedGuesses = ""
        playGame lastDifficulty Running wordToGuess wordToPrint failedGuesses