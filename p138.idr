module Main

import System

--1,2,3

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target tries = do
  putStr ("Number of tries: " ++ cast tries ++ "\n")
  putStr "Insert a number: "
  Just number <- readNumber | Nothing => do putStr "Invalid number\n"
                                            guess target tries
  if number < target
    then do
      putStr "Too small, try again\n"
      guess target (tries + 1)
    else
      if number > target then do
        putStr "Too big, try again\n"
        guess target (tries + 1)
      else do
        putStr "You got it right\n"

main : IO ()
main = do
  random <- time
  guess (1 + (mod (cast random) 100)) 0

--4
my_repl : String -> (String -> String) -> IO ()
my_repl str f = do
  putStr str
  line <- getLine
  putStr (f line ++ "\n")
  my_repl str f

my_replWith : a -> String -> (a -> String -> Maybe (String, a)) -> IO ()
my_replWith val prompt f = do
  putStr prompt
  input <- getLine
  case f val input of
    Just (response, new_val) => do
      putStr (response ++ "\n")
      my_replWith new_val prompt f
    Nothing => pure ()
