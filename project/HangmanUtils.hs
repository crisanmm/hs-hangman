module HangmanUtils (
    myGetChar,
    myPutStr,
    replaceCharAtIndex,
    _areEqual,
    areEqual,
    splitBy
) where

import System.IO 

myGetChar :: IO Char
myGetChar = do
                myPutStr "Enter character: "
                input <- getLine
                if null input then
                    myGetChar
                else
                    return $ head input

myPutStr :: String -> IO ()
myPutStr str = do
                putStr str
                hFlush stdout

replaceCharAtIndex :: Int -> Char -> String -> String
replaceCharAtIndex index char string = (take index string) ++ (char:[]) ++ (drop (index + 1) string)

type WordToGuess = [Char];
type WordToPrint = [Char];

_areEqual :: Int -> WordToGuess -> WordToPrint -> Bool
_areEqual index wordToGuess wordToPrint
    | index >= (length wordToGuess) = True
    | (wordToGuess !! index) == (wordToPrint !! (index*2)) = _areEqual (index + 1) wordToGuess wordToPrint
    | otherwise = False

areEqual :: WordToGuess -> WordToPrint -> Bool
areEqual = _areEqual 0

splitBy :: Char -> String -> [String]
splitBy _ "" = [];
splitBy delimiterChar inputString = foldr f [""] inputString
  where f :: Char -> [String] -> [String]
        f currentChar allStrings@(partialString:handledStrings)
          | currentChar == delimiterChar = "":allStrings -- start a new partial string at the head of the list of all strings
          | otherwise = (currentChar:partialString):handledStrings -- add the current char to the partial string