import System.IO
import Data.List
import qualified Data.Char as C (toLower)
import System.Random (randomRIO)

separate :: String -> [String]
separate x = case dropWhile (==',') x of
                 "" -> []
                 word -> x : separate xs
                     where (x, xs) = break (==',') word
                     -- Thanks source!

parse :: String -> [[String]]
parse = map separate . lines . filter (/='\r')

select :: [a] -> IO a
select xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

roll :: Int -> IO (Int, Int)
roll x = do
    a <- randomRIO (0, x - 1)
    b <- randomRIO (0, x - 1)
    if a == b then roll x else return (a,b)

toLower :: String -> String
toLower = map C.toLower

guess :: [[String]] -> IO ()
guess l@(x:xs) = do
    word <- select xs
    lang <- roll $ length word
    let (a,b) = lang in do
        putStr $ "Type \"" ++ (word !! a) ++ "\" in " ++ (x !! b) ++ ": "
        hFlush stdout
        input <- getLine
        if input == "quit" then
            return ()
        else do
            putStrLn $ if (toLower input) == (word !! b) then "Correct!" else "Incorrect."
            guess l
    return ()

main = do
    inFile <- readFile "dict.txt"
    putStrLn "Type \"quit\" to exit.\n"
    guess (parse inFile)
    return ()
