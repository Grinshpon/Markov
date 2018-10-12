-- Markov: Haskell Powered Calculator
--Daniel Grinshpon
data Symbol = Add | Sub | Mult | Div | Pow deriving (Show)
data Container = Bracket | Brace | Paren | EndSub deriving (Show)
data Name = Quit deriving (Show)

data Token = Keysymbol Char | Identifier String | Statement String | Open Container | Close Container deriving (Show) -- value/number
-- integrals/derivatives/etx will be KEYWORDS
data Tree = Operator Symbol | Argument Tree | Number Float | Variable String Float | Term [Tree] | Command Name deriving (Show) -- ??? Parser Tree not finalized

lexer :: String -> [Token] -- lexer currently shares functionality of parser, break down further into keywords/identifiers?
lexer [] = []
lexer (' ':str) = lexer str --ignore whitespace
--lexer (x:' ':str) = lexer (x:str)
lexer ":q" = [Statement ":q"]
lexer (d:str)   | d `elem` "^*/+-" = (Keysymbol d):(lexer str)
                | d == '(' = (Open Paren):(lexer str)
                | d == '[' = (Open Brace):(lexer str)
                | d == '{' = (Open Bracket):(lexer str)
                | d == ')' = (Close Paren):(lexer str)
                | d == ']' = (Close Brace):(lexer str)
                | d == '}' = (Close Bracket):(lexer str)
                | otherwise = case lexer str of
    ((Identifier n):xs) -> (Identifier (d:n)):xs --Will need to differentiate fractions/decimals [55 or 5.55, 5.5.5 returns error] (and variables [x,y,...])
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
parse [Statement cmd]   | cmd == ":q" = [Command Quit]
                        | otherwise = error "Unkown Command"
parse ((Open _):tkns) = subParse tkns
parse ((Close _):tkns) = subParse ((Close EndSub):tkns)
parse ((Keysymbol d):tkns)  | d == '+' = (Operator Add):(parse tkns)
                            | d == '-' = (Operator Sub):(parse tkns)
                            | d == '*' = (Operator Mult):(parse tkns)
                            | d == '/' = (Operator Div):(parse tkns)
                            | d == '^' = (Operator Pow):(parse tkns)
parse ((Identifier x):tkns) = (Variable x 0):(parse tkns) -- NOT COMPLETE: PLACEHOLDER EVAL

 --test
main = do
let inp = ">> " --eventually out[0..] stored in list so you can plug in answers
let out = "= "
print (lexer "68 * (5+1)-5.5/1")
print (lexer ":q")
print (parse (lexer "2*((3-4)/(5+6))"))
