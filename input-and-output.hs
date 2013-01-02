-- **** Hello world
-- :t putStrLn has a type of "IO ()"
-- :t getStrLen has a type of "IO String"

-- IO actions can be executed in the main function or in another IO action (or in ghci)

-- another IO action is performed via do syntax
-- IO actions execute and return something only inside IO, can bind the result via <-
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

main10 = do
  line <- getLine
  if null line
     then return ()
     else do
         putStrLn $ reverseWords line
         main1

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
