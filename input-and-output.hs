


main1 = do
  line <- getLine
  if null line
     then return ()
     else do
         putStrLn $ reverseWords line
         main1

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
