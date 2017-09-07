module SqlParser where

main = runParser "SELECT * FROM Table"

-- SELECT Field1, Field2 FROM Table
-- INSERT Table (Field1, Field2) VALUES ("Value1", 2)
runParser :: String -> SelectAst
runParser s = case words s of
    ("SELECT":xs) -> select xs
    _ -> SelectAst { fields = [], table = "" }

data SelectAst = SelectAst
    { fields :: [String]
    , table :: String
    } deriving (Show)

select :: [String] -> SelectAst
select ast = SelectAst { fields = ["kjfkds"], table = "" }

