--1
-- printLonger : IO ()
-- printLonger = do
--   putStr "First string: "
--   str1 <- getLine
--   putStr "Second string: "
--   str2 <- getLine
--   putStr (show (max (length str1) (length str2)))

--2
printLonger : IO ()
printLonger =
  putStr "First string: " >>= \_ =>
  getLine >>= \str1 =>
  putStr "Second string: " >>= \_ =>
  getLine >>= \str2 =>
  putStr (show (max (length str1) (length str2)))
