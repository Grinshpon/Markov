-- Markov: Haskell Powered Calculator
--Daniel Grinshpon
module Markov where

data Symbol = Add | Sub | Mult | Div | Pow deriving (Show,Eq)
data Container = Bracket | Brace | Paren | EndSub deriving (Show,Eq)
data Name = Quit | Save | Recalc deriving (Show, Eq)
data Id = Number Double | Variable String Double deriving(Show,Eq)

data Token = Keysymbol Char | Identifier String | Statement String | Open Container | Close Container deriving (Show) -- value/number
-- integrals/derivatives/etx will be KEYWORDS
data Tree = Operator Symbol | Argument Tree | Value Id | Term [Tree] | Command Name deriving (Show) -- ??? Parser Tree not finalized

contains :: String -> String -> Bool
contains _ [] = False
contains [] _ = True
contains (m:ain) check = (m `elem` check) && (ain `contains` check)

--hasOne :: String -> Char -> Bool
--hasOne x c =

--toDouble :: String -> Double
--toDouble x   | x `hasOne` '.' = read x :: Double
            -- | otherwise = error "Parse Error: Double Error: Cannot Convert To Number"

toDouble :: String -> Double
toDouble x = read x :: Double

lexer :: String -> [Token]
lexer [] = []
lexer (' ':str) = lexer str --ignore whitespace
--lexer (x:' ':str) = lexer (x:str)
lexer (':':s) = [Statement s]
lexer (d:str)   | d `elem` "^*/+-" = (Keysymbol d):(lexer str)
                | d == '(' = (Open Paren):(lexer str)
                | d == '[' = (Open Brace):(lexer str)
                | d == '{' = (Open Bracket):(lexer str)
                | d == ')' = (Close Paren):(lexer str)
                | d == ']' = (Close Brace):(lexer str)
                | d == '}' = (Close Bracket):(lexer str)
                | otherwise = case lexer str of
    ((Identifier n):xs) -> (Identifier (d:n)):xs --Will need to differentiate fractions/decimals [5.5.5 returns error] (and variables [x,y,..])
    xs -> (Identifier [d]):xs

strip :: [Token] -> Int -> [Token]
strip tkns 0 = tkns
strip ((Open x):tkns) n = strip tkns (n+1)
strip ((Close x):tkns) n = strip tkns (n-1)
strip (t:ts) n = strip ts n

subParse :: [Token] -> [Tree]
subParse ((Close EndSub):_) = []
subParse tkns = (Term (parse tkns)):(parse (strip tkns 1))

parse :: [Token] -> [Tree]
parse [] = []
parse [Statement cmd]   | cmd == "q" = [Command Quit]
                        | cmd == "s" = [Command Save]
                        | cmd == "r" = [Command Recalc]
                        | otherwise = error "Parse Error: Unkown Command"
parse ((Open _):tkns) = subParse tkns
parse ((Close _):tkns) = subParse ((Close EndSub):tkns)
parse ((Keysymbol d):tkns)  | d == '+' = (Operator Add):(parse tkns)
                            | d == '-' = (Operator Sub):(parse tkns)
                            | d == '*' = (Operator Mult):(parse tkns)
                            | d == '/' = (Operator Div):(parse tkns)
                            | d == '^' = (Operator Pow):(parse tkns)
parse ((Identifier (x:xs)):tkns)    | (x:xs) `contains` "1234567890." = (Value (Number (toDouble (x:xs)))):(parse tkns)
                                    | not (x `elem` "1234567890.") = (Value (Variable (x:xs) 0.0)):(parse tkns) -- NO FUNCTIONALITY FOR VARIABLES YET
                                    | otherwise = error "Parse Error: Invalid Identifier"
--parse ((Identifier x):tkns) = (Variable x 0):(parse tkns) -- NOT COMPLETE: PLACEHOLDER EVAL

-- write preEvaluate to determine whether something is an equation or function or command etc.
preEvaluate :: [Tree] -> String
preEvaluate [] = ""
preEvaluate [Command cmd] = show cmd
preEvaluate x = show $ evaluate x

interpret :: Tree -> Double
interpret (Value (Number n)) = n
interpret (Value (Variable x n)) = n -- NO FUNCTIONALITY FOR VARIABLES YET
interpret (Term t) = evaluate t
interpret _ = error "Interpret Error: Incomplete Input"

evaluate :: [Tree] -> Double
evaluate [] = 0 -- should not be matched
evaluate [t] = interpret t
evaluate (n:(Operator op):ts)   | op == Add = (interpret n) + (evaluate ts)
                                | op == Sub = (interpret n) - (evaluate ts)
                                | op == Mult = (interpret n) * (evaluate ts)
                                | op == Div = (interpret n) / (evaluate ts)
                                | op == Pow = (interpret n) ** (evaluate ts)
evaluate _ = error "Evaluate Error: Incomplete Input"

--output :: a -> String
output :: String -> String
output input = preEvaluate $ parse $ lexer input
