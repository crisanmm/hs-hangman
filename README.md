# EN

# Hangman

## The problem that has been solved
Implementing the classic pen and paper game into a computer program.

As an addition, the game has been implemented in three difficulties:
* **Easy** - allows a maximum of 12 mistakes, previous mistakes will be shown.
* **Medium** - allows a maximum of 7 mistakes, previous mistakes will be shown.
* **Hard** - allows a maximum of 4 mistakes, previous mistakes <ins>**will not**</ins> be shown.

## Data formats

For representing the game's difficulty a new data type `GameDifficulty` has been created, as shown below:
```haskell
data GameDifficulty = Easy | Medium | Hard
```

Similarly was done for representing the game's state with the `GameState` data type:
```haskell
data GameState = Running | Finished
```

Game related information was represented using type synonyms:

```haskell
type WordToGuess = [Char] -- the word that has to be guessed
type WordToPrint = [Char] -- the word that is shown to the users
type FailedGuesses = [Char] -- the guesses that were incorrect
type GuessChar = Char -- the most recent guess
```

## Difficult/interesting project aspects

At first sight printing the diagram after every guess can seem hard, especially modifying it for adding a new part to the stick figure as a consequence of an incorrect guess.

In order to prevent this difficulty I chose to create a new file corresponding to each diagram that can be printed, after which based on the picked difficulty and the number of mistakes I read and print the appropriate file. Of course it would be more efficient if there was no I/O operations, but I personally think this sacrifice is worht it. Alternatively the diagrams can be implemented inside the program so that there won't be any I/O overhead. 

The structure of the directory where I stored these files is shown below, the first file within the difficulty represents an initial diagram, without any mistakes, and the last file represents a final diagram, where the stick figure is completed.

```
── difficulty
   ├── easy
   │   ├── easy0.txt
   │   ├── ...
   │   └── easy12.txt
   ├── medium
   │   ├── medium0.txt
   │   ├── ...
   │   └── medium7.txt
   └── hard
       ├── hard0.txt
       ├── ...
       └── hard4.txt
```

I have also created a file that stores the last chosen difficulty in this directory so that you don't have choose a difficulty every time you want to play the game.

# RO

# Spânzurătoarea

## Problema rezolvată
Implementarea jocului din varianta clasica in hârtie și creion in varianta pe calculator.

Adițional functionalitații de baza a jocului au fost adaugate dificultați:
* **Easy** - permite un maxim de 12 greșeli, cu afișarea caracterelor greșite anterior.
* **Medium** - permite un maxim de 7 greșeli, cu afișarea caracterelor greșite anterior.
* **Hard** - premite un maxim de 4 greșeli, <ins>**fara**</ins> afișarea caracterelor greșite anterior.

## Modul de reprezentare a datelor

Pentru a reprezenta dificultatea jocului s-a creat un nou tip de date `GameDifficulty` ca mai jos:
```haskell
data GameDifficulty = Easy | Medium | Hard
```

Asemănător s-a procedat pentru a reprezenta starea jocului cu tipul de date `GameState`:
```haskell
data GameState = Running | Finished
```

Informațiile legate de joc au fost reprezentate folosind type synonyms:

```haskell
type WordToGuess = [Char] -- cuvântul ce trebuie ghicit
type WordToPrint = [Char] -- cuvântul care este afisat utilizatorului
type FailedGuesses = [Char] -- caracterele greșite 
type GuessChar = Char -- cea mai noua ghicire
```

## Aspecte dificile/interesante din proiect

La o primă vedere afișarea diagramei dupa fiecare ghicire poate părea greu, mai ales modificarea acesteia pentru a adăuga o noua parte figurei ca rezultat a unei greseli in ghicire.

Pentru a preveni această dificultate am ales sa creez un fișier ce corespunde fiecărei diagrame ce poate fi afișată, dupa care in functie de dificultatea aleasă și numărul de greșeli citesc și afisez fișierul potrivit. Bineînteles că ar fi fost mai eficient dacă nu ar exista operații I/O, insă acesta este un sacrifiu care personal cred că s-a meritat. Alternativ diagramele pot fi implementate in program pentru a nu exista overhead I/O.

Structura directorului unde am stocat aceste fișiere este prezentată mai jos, primul fișier din cadrul fiecarei dificultăți reprezintă o diagramă inițiala, făra greșeli, iar ultimul fișier o diagramă finala, in care figura este completată.

```
── difficulty
   ├── easy
   │   ├── easy0.txt
   │   ├── ...
   │   └── easy12.txt
   ├── medium
   │   ├── medium0.txt
   │   ├── ...
   │   └── medium7.txt
   └── hard
       ├── hard0.txt
       ├── ...
       └── hard4.txt
```

Tot in acest director se află și un fișier ce memoreaza ultima dificultate aleasă pentru a nu fi necesară alegerea ei de fiecare dată cand se doreste a juca jocul.