import System.IO
import Data.List
import System.Random (randomRIO)

separate :: String -> [String]
separate x = case dropWhile (==',') x of
                 "" -> []
                 word -> w : separate xs
                     where (w, xs) = break (==',') word
                     -- Thanks source!

parse :: String -> [[String]]
parse = map separate . lines . filter (/='\r')

select :: [a] -> IO a
select xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

main = do
    inFile <- readFile "something.txt"

    return ()
