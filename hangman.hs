import Data.Char (toUpper, toLower)
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad (when)
import System.IO 

-- fillLetterInWord :: Char -> [Char] -> [Char]
-- fillLetterInWord letter word = 

----------------- UTILITY -----------------
myGetChar :: IO Char
myGetChar = do
                input <- getLine
                return $ head input

myPutStr :: String -> IO ()
myPutStr str = do
                putStr str
                hFlush stdout

replaceCharAtIndex :: Int -> Char -> String -> String
replaceCharAtIndex index char string = (take index string) ++ (char:[]) ++ (drop (index + 1) string)
----------------- UTILITY -----------------

-- data GameString = GSWordToGuess [Char] |
--                 GSWordToPrint [Char] |
--                 GSFailedGuesses [Char] |
--                 GSGuessChar Char 
--                 deriving (Show)

-- data WordToGuess = WordToGuess [Char];
-- data WordToPrint = WordToPrint [Char];
-- data FailedGuesses = FailedGuesses [Char];
-- data GuessChar = GuessChar Char;

data GameState = Running | Finished deriving (Show, Eq)

type WordToGuess = [Char];
type WordToPrint = [Char];
type FailedGuesses = [Char];
type GuessChar = Char;

printGraphic :: WordToPrint -> FailedGuesses -> IO ()
printGraphic wordToPrint failedGuesses
    = do
        graphic <- readFile $ "difficulty/hard/hard" ++ (show $ length failedGuesses) ++ ".txt"
        putStrLn "" -- padding
        putStrLn graphic
        putStrLn "" -- padding
        putStrLn wordToPrint
        putStrLn "" -- padding
        putStrLn $ "Failed guesses: " ++ failedGuesses
        putStrLn "" -- padding

-- checkCharInWord :: GuessChar -> WordToGuess -> Char
-- checkCharInWord foldr (\x acc -> (||) (x==char) acc) False word


_getNewWordToPrint :: Int -> WordToGuess -> WordToPrint -> GuessChar -> WordToPrint
_getNewWordToPrint index wordToGuess wordToPrint guessChar
    | index >= (length wordToGuess) = wordToPrint
    | (wordToGuess !! index == guessChar) = _getNewWordToPrint (index + 1) wordToGuess (replaceCharAtIndex index guessChar wordToPrint) guessChar
    | otherwise = _getNewWordToPrint (index + 1) wordToGuess wordToPrint guessChar
 
getNewWordToPrint  :: WordToGuess -> WordToPrint -> GuessChar -> WordToPrint
getNewWordToPrint = _getNewWordToPrint 0
      
getNewFailedGuesses :: WordToGuess -> GuessChar -> FailedGuesses -> FailedGuesses
getNewFailedGuesses wordToGuess guessChar failedGuesses
    | not (elem guessChar wordToGuess) && not (elem guessChar failedGuesses) = failedGuesses ++ (guessChar:[])
    | otherwise = failedGuesses
                                
playGame :: GameState -> WordToGuess -> WordToPrint -> FailedGuesses -> IO ()
playGame gameState wordToGuess wordToPrint failedGuesses = 
        if gameState == Running then
            do
                printGraphic wordToPrint failedGuesses
                myPutStr "Enter character: "
                guessChar <- myGetChar
                -- putChar guessChar
                let newWordToPrint = getNewWordToPrint wordToGuess wordToPrint guessChar
                let newFailedGuesses = getNewFailedGuesses wordToGuess guessChar failedGuesses
                if wordToGuess == newWordToPrint then
                    playGame Finished wordToGuess newWordToPrint newFailedGuesses
                else 
                    playGame Running wordToGuess newWordToPrint newFailedGuesses
        else
            do
                printGraphic wordToPrint failedGuesses
                putStrLn "You won."

main :: IO ()
main = do
        myPutStr "Enter word to guess: "
        wordToGuess <- getLine
        let wordToPrint = foldr (\x acc -> "_" ++ acc) [] wordToGuess
        let failedGuesses = ""
        playGame Running wordToGuess wordToPrint failedGuesses
