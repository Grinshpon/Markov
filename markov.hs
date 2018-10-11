-- Markov: Haskell Powered Calculator
--Daniel Grinshpon
data Symbol = Add | Sub | Mult | Div | Pow deriving (Show)
data Container = Bracket | Brace | Paren deriving (Show)
data Name = Quit deriving (Show)

data Token = Command Name | Open Container | Close Container | Operator Symbol | Number String deriving (Show)

data Tree = Term [Tree] | Factor  deriving (Show)

lexer :: String -> [Token]
lexer [] = []
lexer (' ':str) = lexer str --ignore whitespace
--lexer (x:' ':str) = lexer (x:str)
lexer ":q" = [Command Quit]
lexer ('(':str) = (Open Paren):(lexer str)
lexer (')':str) = (Close Paren):(lexer str)
lexer ('^':str) = (Operator Pow):(lexer str)
lexer ('*':str) = (Operator Mult):(lexer str)
lexer ('/':str) = (Operator Div):(lexer str)
lexer ('+':str) = (Operator Add):(lexer str)
lexer ('-':str) = (Operator Sub):(lexer str)
lexer (d:str) = case lexer str of
    ((Number n):xs) -> (Number (d:n)):xs --Will need to differentiate fractions/decimals [55 or 5.55, 5.5.5 returns error] (and variables [x,y,...])
    xs -> (Number [d]):xs
 
 --test   
main = do
let inp = "in  > " --eventually out[0..] stored in list so you can plug in answers
let out = "out < "
print (lexer "68 * (5+1)-5.5/1")
print (lexer ":q")