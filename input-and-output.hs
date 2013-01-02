-- **** Hello world
main1 = putStrLn "Hello, world!"
-- :t putStrLn has a type of IO ()
-- IO is an action with some result type, () in this case
-- IO is run when given a name main and the program is run

--do syntax
--each step is an IO action
--result of IO action is the result of the last step
-- <- binds IO action result to a name
main2 = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn ("Hey " ++ name ++ ", you rock!")

main3 = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn $ "Read this carefully, because this is your future: " ++ tellFortune name

tellFortune :: String -> String
tellFortune name = "You'll become a rich and successful man!"

main10 = do
  line <- getLine
  if null line
     then return ()
     else do
         putStrLn $ reverseWords line
         main1

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
