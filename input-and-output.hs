import Data.Char
import System.IO
import System.Random
import qualified Data.ByteString.Lazy as B  
import qualified Data.ByteString as S  
import System.Environment  
import System.IO.Error 

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

-- openFile signature: FilePath -> IOMode -> IO Handle
-- type FilePath = String
-- IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
-- hGetContents is just like getContents but with a handle
main14 = do
  handle <- openFile "girlfriend.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

-- withFile does all that job
-- withFile :: FilePath -> IOMode -> (IOHandle -> IO a) -> IO a
main15 = do
  withFile "girlfriend.txt" ReadMode (\handle -> do contents <- hGetContents handle; putStr contents)

-- wow, let's implement withFile1
withFile1 :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile1 path mode f = do
  handle <- openFile path mode
  result <- f handle
  hClose handle
  return result

main16 = do withFile "girlfriend.txt" ReadMode (\handle -> do contents <- hGetContents handle; putStr contents)

-- also, consider functions hGetLine, hPutStr, hPutStrLn, hGetChar
-- readFile, writeFile, appendFile
-- hSetBuffering :: Handle -> BufferMode -> IO ()
-- type BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)
-- hFlush
-- openTempFile :: FilePath -> String -> IO (FilePath, Handle)

main17 = do
  withFile "something.txt" ReadMode (\handle -> do
                                       hSetBuffering handle $ BlockBuffering (Just 2048)
                                       contents <- hGetContents handle
                                       putStr contents)




-- **** Command line arguments
-- System.Environment:
-- getArgs :: IO[String]
-- getProgName :: IO String




-- **** Randomness
-- System.Random.random :: (RandomGen g, Random a) => g -> (a, g)
-- mkStdGen :: Int -> StdGen
genRandInt = random $ mkStdGen 100 :: (Int, StdGen)
getRandFloat = random $ mkStdGen 100 :: (Float, StdGen)

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, _) = random newGen'
    in (firstCoin, secondCoin, thirdCoin)

-- let's use randoms :: (RandomGen g, Random a) => g -> [a]
-- first let's implement it:
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value : randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n - 1) newGen
    in (value:restOfList, finalGen)

-- random in range: randomR, randomRs
rrr = randomR (1,6) (mkStdGen 359353) :: (Int, StdGen)

-- getStdGen :: IO StdGen
main18 = do
  gen <- getStdGen
  putStr $ take 20 $ randomRs ('a', 'z') gen

-- but getStdGen returns the same generator
-- so use newStdGen after getStdGen (returns new gen and updates the global one) or generate infinite list based on stdGen

  


-- **** Bytestrings
-- String is [Char], Char size is not fixed and lists are lazy, reasons why bytestrings exist
-- Data.ByteString - no laziness
-- Data.ByteString - lazy, but evaluated by chunks

-- B.pack :: [Word8] -> ByteString
pack1 = B.pack [99, 97, 110]
pack2 = B.pack [98..120]
-- B.pack, S.pack, B.unpack, S.unpack
-- B.fromChunks - strict bytestreams to lazy; toChunks - back
-- B.cons, B.cons'
-- empty creates an empty bytestring
-- readFile, writeFile




-- **** Exceptions
-- impure code throws exceptions, IO errors for instance
-- pure code throws exceptions, div by 0 for instance
-- exceptions may only be caught in IO part of code, because the code is lazy and only proved to evaluate in IO

-- catch function - catch :: IO a -> (IOError -> IO a) -> IO a
-- ioError :: IOException -> IO a   -   IO anything, actually
-- 'a' is '()' in the following example

main19 = toTry `catch` handler  
              
toTry :: IO ()  
toTry = do (fileName:_) <- getArgs  
           contents <- readFile fileName  
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
  
handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
    | otherwise = ioError e  