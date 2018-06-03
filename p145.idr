import Data.Vect

--1
readToBlank : IO (List String)
readToBlank = do
  input <- getLine
  if input == ""
    then pure []
    else do
      rest <- readToBlank
      pure (input :: rest)

--2
readAndSave : IO ()
readAndSave = do
  lines <- readToBlank
  filename <- getLine
  Right _ <- writeFile filename
                       (foldr (\line, acc => line ++ "\r\n" ++ acc) "" lines)
    | Left err => putStrLn (show err)
  pure ()


--3
readVect : File -> IO (len ** Vect len String)
readVect file = do
  end <- fEOF file
  if end
    then pure (0 ** [])
    else do
      Right line <- fGetLine file
      (_ ** lines) <- readVect file
      pure (_ ** line :: lines)

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  Right file <- openFile filename Read | Left err => do putStrLn (show err)
                                                        pure (0 ** [])
  readVect file
