-- Markov: Haskell Powered Calculator
--Daniel Grinshpon
import System.IO
import System.Exit
import Markov

input :: [Char] -> IO ()
input " " = do
    putStrLn "-----------------------------\n|-----Markov Calculator-----|\n-----------------------------"
    input "> "
input x = do
    putStr "> "
    hFlush stdout
    inpt <- getLine
    let out = output inpt
    putStrLn out
    if out == "Quit"
        then exitSuccess
    else input out --eventually the previous 'out' will be able to be used as an ANS in a next expression

main = input " "
