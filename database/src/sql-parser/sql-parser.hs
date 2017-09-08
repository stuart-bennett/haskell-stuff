module SqlParser where

main = runParser "SELECT * FROM Table"

-- SELECT Field1, Field2 FROM Table
-- SELECT <query specification>
-- <query specification> ::=
--   SELECT [<set quantifier>] <select list> <table expression>
-- <select list> ::=
--     <asterisk>
--   | <select sublist>[{<comma> <select sublist>}... ]
-- <select sublist> ::= <derived column> | <qualifier> <period> <asterisk>
-- <derived column> ::= <value expression> [<as clause>]
-- <as clause> ::= [AS] <column name>
-- <value expression> ::=
--     <numeric value expression>
--   | <string expression>
--   | <datetime value expression>
--   | <interval value expression>
-- <string value expression> ::= <character value expression> | <bit value expression>
-- 
runParser :: String -> [Token String]
runParser s = reverse $ lexer s [] []

data Token s = Space | Word s deriving (Show)

lexer :: String -> String -> [Token String] -> [Token String]
lexer s cs t = case s of
    []       -> t
    (' ':xs) -> lexer xs [] (Word cs: t)
    (x:xs)   -> lexer xs (cs++[x]) t
