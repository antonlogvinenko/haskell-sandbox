import Data.Char
import System.IO

-- **** Hello world
-- :t putStrLn has a type of "IO ()"
-- :t getStrLen has a type of "IO String"

-- IO actions are executed in the main function or in another IO action (or in ghci)
-- another IO action is performed via do syntax

-- when in IO action or main or ghci:
-- IO actions execute and return "IO m"
-- can bind the result via <-
-- "return m" may be used to create an IO action "IO m"
-- all steps are IO actions, all except the last one are allowed to bind
-- the last step is the result of the containing IO action


main1 = putStrLn "Hello, world!"
main2 = do
  putStrLn "Hello, what's your name?"
  getLine
  putStrLn ("Hey "  ++ ", you rock!")

main3 = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn $ "Read this carefully, because this is your future: " ++ tellFortune name

tellFortune :: String -> String
tellFortune name = "You'll become a rich and successful man!"

-- The following is invalid: the second argument has a type of IO String
-- So impure environment required to extract the value
-- nameTag = "Hello, my name is " ++ getLine

main4 = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <-getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $ "Hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

main5 = do
  line <- getLine
  if null line
     then return ()
     else do
         putStrLn $ reverseWords line
         main5

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- putStr, putChar, print, getChar, when, sequence, mapM, mapM_, forever, forM

-- sequence makes an IO action from a list of IO actions - for it to be an executable IO action
sequenceTest = sequence $ map print [1, 2, 3]
-- printed result is [(), (), ()] - concatenaited results of IO acions
-- result is printed for evaluated IO actions in ghci if only it is not ()




-- **** Files and streams
-- getContents reads standard input until EOF is encountered
-- getContents :: IO String
-- String is [Char], lazy
main6 = do
  contents <- getContents
  putStr $ map toUpper contents

main7 = do
  contents <- getContents
  putStr $ shortLinesOnly contents

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in result

-- function interact does just that: interact :: (String -> String) -> IO ()
main8 = interact shortLinesOnly

-- or even shorter:
main9 = interact $ unlines . filter ((<10) . length) . lines
-- or
main10 = let filtering = (< 10) . length
         in interact $ unlines . filter filtering . lines
--or
main11 = interact $ unlines . filter filterFun . lines
         where filterFun = (< 10) . length

main12 = interact respondPalindromes
respondPalindromes :: String -> String
respondPalindromes contents = unlines (map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))
                              where isPalindrome xs = xs == reverse xs
main13 = interact respondPalindromes2
respondPalindromes2 :: String -> String
respondPalindromes2 = unlines . (map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome")) . lines
                               where isPalindrome xs = xs == reverse xs


main14 = do
  handle <- openFile "girlfriend.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

-- **** Command line arguments
-- **** Randomness
-- **** Bytestrings
-- **** Exceptions