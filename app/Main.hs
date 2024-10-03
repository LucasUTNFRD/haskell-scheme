module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Except

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

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = 
     do result <- eval pred
        case result of
             Bool False -> eval alt
             otherwise  -> eval conseq

eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


-- TODO 
-- implement cdr 
-- implemt car


-- This function returns the first element of a list. For example:
-- (car '(a b c)) = a
-- (car '(a)) = a
-- (car '(a b . c)) = a
-- (car 'a) = error – not a list
-- (car 'a 'b) = error – car only takes one argument
car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList(x:xs) _] = return x
car [badArg]  = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList
 
-- (cdr '(a b c)) = (b c)
-- (cdr '(a b)) = (b)
-- (cdr '(a)) = NIL
-- (cdr '(a . b)) = b
-- (cdr '(a b . c)) = (b . c)
-- (cdr 'a) = error – not a list 
-- (cdr 'a 'b) = error – too many arguments
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList[_] x] = return x
cdr [DottedList(_:xs) x] = return $ DottedList xs x
cdr [badArg]  = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList


cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List (x:xs)


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car",car),
              ("cdr",cdr),
              ("cons",cons)]

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do 
                                left <- unpacker $ args !! 0
                                right <- unpacker $ args !! 1
                                return $ Bool $ left `op` right

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)


-- ######### unpack functions #########
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr:: LispVal -> ThrowsError String 
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show  s

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

-- ######### show functions #########
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"


showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError


extractValue :: ThrowsError a -> a
extractValue (Right val) = val
-- this definiton doesnt includes any args. This is an expalme of point-free style
-- Purely in terms of function composition and partial applicatio
unwordsList :: [LispVal]-> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal
type ThrowsError = Either LispError

trapError action = catchError action (return . show)

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
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwError $ Parser err
     Right val -> return val


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
     args <- getArgs
     evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled

--     (expr:_) <- getArgs
--     pumain :: IO ()
