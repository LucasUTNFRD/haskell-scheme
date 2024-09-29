module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             | QuasiQuote LispVal -- represent an expression quoted with `.
             | Unquote LispVal -- represent an expresison unquoted wiht ,.
             | UnquoteSplicing LispVal -- represent an expression spliced into a list with ,@.

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply "+" args = Number $ foldl1 (+) $ map unpack args
apply "-" args = Number $ foldl1 (-) $ map unpack args
apply "*" args = Number $ foldl1 (*) $ map unpack args
apply "/" args = Number $ foldl1 div $ map unpack args
apply func _ = error $ "Unknown function: " ++ func
--
unpack :: LispVal -> Integer 
unpack (Number n) = n
unpack (List [n]) = unpack n
unpack _ = error "Expected a number"

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

-- this definiton doesnt includes any args. This is an expalme of point-free style
-- Purely in terms of function composition and partial applicatio
unwordsList :: [LispVal]-> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal


-- TODO 
-- implement Float constructor to LispValm abd support R5R5 syntaax for decimals (done). 
--Change parseNumber to support the Scheme standard for different bases. You may find the readOct and readHex functions useful.

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


spaces :: Parser()
spaces = skipMany1 space

-- use >> if the actions don't return a value,
-- >>= if you'll be immediately passing that value into the next action, 
-- do-notation otherwise. 
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

parseString :: Parser LispVal
parseString = do 
                char '"'
                x <- many (escapedChar <|> noneOf "\"")
                char '"'
                return $ String x

escapedChar :: Parser Char 
escapedChar = do 
                char '\\' -- parse the backslash
                c <- oneOf "\\\"nrt"
                return $ case c of
                        'n'  -> '\n'
                        'r'  -> '\r'
                        't'  -> '\t'
                        '\\' -> '\\'
                        '"'  -> '"'  -- Escape the double quote

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom  -- Default case for any other atom

-- many1 is a parser combinator that parses one or more digits

parseFloat :: Parser LispVal
parseFloat = do 
            firstDigit <- many1 digit
            fracPart <- option "" $ do
              char '.' --decimal part
              decimal <- many1 digit
              return ('.':decimal)
            let number = firstDigit ++ fracPart 
            return $ (Float . read) number

-- parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit

-- do notation implementation
parseNumber :: Parser LispVal
parseNumber = do 
              digits <- many1 digit
              return $ (Number . read) digits

parseList :: Parser LispVal
parseList = do 
            expr <- sepBy parseExpr spaces
            return $ List  expr

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiqoute :: Parser LispVal
parseQuasiqoute = do
                  char '`'
                  x <- parseExpr 
                  return $ QuasiQuote x


parseUnquote:: Parser LispVal
parseUnquote = do
                  char ','
                  x <- parseExpr 
                  return $ Unquote x

parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do
    string ",@"  -- Look for ,@
    x <- parseExpr  -- Parse the expression after ,@
    return $ UnquoteSplicing x
--explicit >>= binding 
-- parseNumber :: Parser LispVal
-- parseNumber = many1 digit >>= \x -> return $ (Number . read) x

-- the try combinator attempts to run specified parser, but if it fails, it backs up to the previous state
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseFloat 
         <|> parseQuoted 
         <|> parseQuasiqoute
         <|> parseUnquote
         <|> try parseUnquoteSplicing 
         <|> do char '('
                x <- try parseList  <|> parseDottedList  -- backtracking
                char ')'
                return x


main :: IO ()
main = do 
         (expr:_) <- getArgs
         putStrLn $ show $ eval $ readExpr expr
--     (expr:_) <- getArgs
--     pumain :: IO ()
