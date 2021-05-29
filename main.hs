{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}


import Data.Char
import System.IO
import System.Environment
import System.Directory
import System.Exit


----------------------------------------------------------------------------------------------------------
----------------------------------  Grammar data
----------------------------------------------------------------------------------------------------------

data Cell 
    = Konv   Char -- A-Z
    | Dir    Int  -- [Int]
    | NonDir Cell -- [Cell]
  deriving (Eq, Show)

data Expr
    = Lit Int -- Int
    | Var Cell -- Cell
    | BinOp String  Expr Expr -- Expr String Expr -- String = + - * /
  deriving (Eq, Show)

data Cond 
    = CmpOp String Expr Expr -- Expr String Expr -- String = == != > >= < <= 
    | LogicalOp String Cond Cond -- Cond String Cond -- String = AND OR
  deriving (Eq, Show)

data Instruction
    = Assign Cell Expr         -- Cell <- Expr
    | If Cond Instruction      -- If Cond do Instruction
    | Point String Instruction -- String: Instruction
    | Goto String              -- Goto -> String
    | Input Cell               -- Input: Cell
    | Output Expr              -- Output: Cell
    | Halt                     -- Halt
  deriving (Eq, Show)

data Program = Main [Instruction]
  deriving (Eq, Show)

----------------------------------------------------------------------------------------------------------
-----------------------------------------  Parser
----------------------------------------------------------------------------------------------------------

data ParseError = ParseError
    { errorExpected :: String
    , errorFound :: String
    }


instance Show ParseError where
    show err = "expected: " <> errorExpected err <> ", but found: " <> errorFound err 


newtype Parser a = Parser {
  runParser :: String -> (String, Either ParseError a)
}




instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ \input ->                   -- vezmeme vstup
        case runParser p input of                   -- pustíme parser p na vstup
            (rest, Right x) -> (rest, Right $ f x)  -- pokud se parsování povedlo, aplikujeme f na výslednou hodnotu
            (rest, Left err) -> (rest, Left err)    -- pokud se nepovedlo, jen předáme error


instance Monad Parser where
    return :: a -> Parser a
    return x = Parser $ \input -> (input, Right x)  -- parser se nedotkl vstupu a uspěl s 'x'
    
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = Parser $ \input ->                    -- vezmeme vstup
        case runParser p input of                   -- pustíme parser na vstup
            (rest, Right x) -> runParser (f x) rest -- pokud se parsování povedlo, pak 'f x :: Parser b', tak jej jen pustíme na zbytek řetězce
            (rest, Left err) -> (rest, Left err)    -- pokud se nepovedlo, jen předáme error


instance Applicative Parser where
    pure = return
    mf <*> mx = do
        f <- mf
        x <- mx
        return $ f x


try :: Parser a -> Parser a
try p = Parser $ \input ->                 -- vezmeme vstup
    case runParser p input of              -- pustíme parser na vstup
        (_, Left err) -> (input, Left err) -- pokud nastala chyba, vrátíme původní vstup a onu chybu
        success       -> success           -- jinak se parsování povedlo, tak pokračujeme dál



(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \input ->
    case runParser p1 input of
        (rest, Left err)
            | rest == input -> runParser p2 input
            | otherwise -> (rest, Left err)
        success -> success


parseError :: String -> String -> Parser a
parseError expected found = Parser $ \input -> 
    (input, Left $ ParseError expected found)


satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy description p = try $ do
    c <- parseAny
    if p c
        then return c
        else parseError description [c]


run :: Parser a -> String -> Either ParseError a
run p s = snd $ runParser go s
  where
     go = do
         result <- p    -- použije parser 'p', tím získá výsledek
         eof            -- naparsuje konec souboru/vstupu
         return result  -- vrátí výsledek parseru 'p'

----------------------------------------------------------------------------------------------------------
-----------------------------------------  Simple parsers
----------------------------------------------------------------------------------------------------------

parseAny :: Parser Char
parseAny = Parser go
  where
    go :: String -> (String, Either ParseError Char)
    go input = case input of 
                 []     -> ("", Left $ ParseError "any character" "end of file")
                 (x:xs) -> (xs, Right x)


eof :: Parser ()
eof = Parser go
 where
    go :: String -> (String, Either ParseError ())
    go input = case input of
                 []    -> (""   , Right ())
                 (c:_) -> (input, Left $ ParseError "end of file" [c])



char :: Char -> Parser Char
char c = satisfy [c] (== c)


isNot :: Char -> Parser Char
isNot c = satisfy ("anything but" ++ [c]) (/= c)


digit :: Parser Char
digit = satisfy "digit" isDigit


many :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do
    first <- p
    rest <- many p
    return (first : rest)


string :: String -> Parser String
string s = try (mapM char s)


number :: Parser Int
number = do
  numericString <- many1 digit  -- načte alespoň jednu cifru
  return $ read numericString   -- použije 'read :: String -> Int' pro konverzi na číslo


between :: Parser a -> Parser c -> Parser b -> Parser b
between left right p = do
    _      <- left
    result <- p
    _      <- right
    return result


parens, brackets :: Parser a -> Parser a
parens   p = try (between (char '(') (char ')') p)
brackets p = try (between (char '[') (char ']') p)


sepBy, sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy  p s = sepBy1 p s <|> return []
sepBy1 p s = do
  first <- p
  rest  <- many (s >>= \_ -> p)   -- stručněji jako 'many (s >> p)'
  return (first : rest)


choice :: String -> [Parser a] -> Parser a
choice desc = foldr (<|>) noMatch
  where 
    noMatch = parseError desc "no match"



----------------------------------------------------------------------------------------------------------
----------------------------------  Parser Cell
----------------------------------------------------------------------------------------------------------

parseCell :: Parser Cell
parseCell = choice "Cell" 
    [ Konv   <$> satisfy "capital letter" isUpper
    , Dir    <$> brackets number
    , NonDir <$> brackets parseCell
    ]
   

----------------------------------------------------------------------------------------------------------
----------------------------------  Parser Expr
----------------------------------------------------------------------------------------------------------

parseAddSub :: Parser Expr
parseAddSub = do
    left  <- parseTerm
    op    <- (string "+" <|> string "-")
    right <- parseExpr
    return (BinOp op left right)

parseMulDiv :: Parser Expr
parseMulDiv = do
    left  <- parseFactor
    op    <- (string "*" <|> string "/")
    right <- parseTerm
    return (BinOp op left right)

parseTerm :: Parser Expr
parseTerm = (try parseMulDiv) <|> parseFactor

parseFactor :: Parser Expr 
parseFactor = choice "Factoring"
    [ parens parseExpr
    , Var <$> parseCell
    , Lit <$> number
    ]

parseExpr :: Parser Expr 
parseExpr =  (try parseAddSub) <|> parseTerm


----------------------------------------------------------------------------------------------------------
----------------------------------  Parser Cond
----------------------------------------------------------------------------------------------------------

parseComp :: Parser Cond
parseComp = do
    left  <- parseExpr 
    op    <- choice "cmp operator" [string "==", string "!=", string "<=", string "<", string ">=", string ">"]
    right <- parseExpr
    return (CmpOp op left right)

parseAnd :: Parser Cond
parseAnd = do
    left  <- parseCondFactor
    op    <- string "AND"
    right <- parseCondTerm
    return (LogicalOp op left right)

parseOr :: Parser Cond
parseOr = do
    left  <- parseCondTerm
    op    <- string "OR"
    right <- parseCond 
    return (LogicalOp op left right)

parseCondFactor :: Parser Cond 
parseCondFactor = parseComp <|> (parens parseCond)

parseCondTerm :: Parser Cond
parseCondTerm = (try parseAnd) <|> parseCondFactor 

parseCond :: Parser Cond
parseCond = (try parseOr) <|> parseCondTerm 


----------------------------------------------------------------------------------------------------------
----------------------------------  Parser Instruction
----------------------------------------------------------------------------------------------------------


parseAssign :: Parser Instruction
parseAssign = do 
    cell <- parseCell
    _    <- string "<-"
    expr <- parseExpr 
    return (Assign cell expr)

parseIf :: Parser Instruction 
parseIf = do 
    _     <- string "If"
    cond  <- parseCond 
    _     <- string "do"
    instr <- parseInstruction
    return (If cond instr)

parsePoint :: Parser Instruction 
parsePoint = do 
    str    <- many (isNot ':')
    _      <- string ":" 
    instr  <- parseInstruction
    return (Point str instr)

parseGoto :: Parser Instruction 
parseGoto = do 
    _   <- string "Goto->"
    str <- many (isNot '\n')
    return (Goto str)

parseInput :: Parser Instruction 
parseInput = do 
    _    <- string "Input"
    _    <- string ":"
    cell <- parseCell
    return (Input cell)

parseOutput :: Parser Instruction 
parseOutput = do 
    _    <- string "Output"
    _    <- string ":"
    expr <- parseExpr
    return (Output expr)

parseHalt :: Parser Instruction
parseHalt = do
    _ <- string "Halt"
    return Halt

parseInstruction :: Parser Instruction 
parseInstruction = choice "Instruction"
    [ try parseAssign
    , try parseIf
    , try parseGoto
    , try parseInput
    , try parseOutput
    , try parsePoint
    , try parseHalt
    ]


----------------------------------------------------------------------------------------------------------
----------------------------------  Parser Program
----------------------------------------------------------------------------------------------------------


parseProgram :: Parser Program 
parseProgram = Main <$> (sepBy parseInstruction (char '\n'))


----------------------------------------------------------------------------------------------------------
-----------------------------------  String trim
----------------------------------------------------------------------------------------------------------


removeComments :: String -> String 
removeComments s = (reverse . snd) (foldl go (False, " ") s)
    where
        go (False, xs) c
            | c == '#'  = (True , xs  )
            | otherwise = (False, c:xs)
        go (True,  xs) c
            | c == '\n' = (False, c:xs)
            | otherwise = (True , xs  )


removeWhiteSpaces :: String -> String 
removeWhiteSpaces = filter (/= ' ')


removeExtraLines :: String -> String
removeExtraLines s = (go' . snd) (foldr go (True, "") s)
    where 
        go c (False, xs)
            | c == '\n' = (True,  c:xs)
            | otherwise = (False, c:xs)
        go c (True, xs)
            | c == '\n' = (True,  xs  )
            | otherwise = (False, c:xs)
        go' [] = []
        go' (x:xs)
            | x == '\n' = xs
            |otherwise  = x:xs

prepareForParsing :: String -> String 
prepareForParsing = removeExtraLines . removeWhiteSpaces . removeComments



----------------------------------------------------------------------------------------------------------
----------------------------------  Compiler
----------------------------------------------------------------------------------------------------------

includes, memorymanage, start, end :: String
includes = "#include <stdio.h>\n#include <stdlib.h>\n\n"
memorymanage = "int get(int idx, int** arr, int* max)\n{if(*max<=idx){*arr=(int*)realloc(*arr,sizeof(int)*2*idx);*max=2*idx;}return idx;}\n\n"
start = "int main(){\n    int* kov_arr=(int*)malloc(sizeof(int)*26);\n    int max=32;\n    int* arr=(int*)malloc(max*sizeof(int));\n"
end = "    free(kov_arr);\n    free(arr);\n}\n"



fromASCII :: Char -> Int
fromASCII c = (ord c) - 65


compileCell :: Cell -> String
compileCell (Konv    c) = "kov_arr[" ++ (show (fromASCII c)) ++ "]"
compileCell (Dir     n) = "*((int*)(get(" ++ (show n) ++ ", &arr, &max) + arr))"
compileCell (NonDir  c) = "*((int*)(get(" ++ (compileCell c) ++ ", &arr, &max) + arr))"


compileExpr :: Expr -> String
compileExpr (Lit int)   = show int
compileExpr (Var cell)  = compileCell cell
compileExpr (BinOp op e1 e2) = "(" ++ (compileExpr e1) ++ op ++ (compileExpr e2) ++ ")"


compileCond :: Cond -> String
compileCond (CmpOp     op e1 e2) = (compileExpr e1) ++ op ++ (compileExpr e2)
compileCond (LogicalOp op c1 c2)
    | op =="AND" = "(" ++ (compileCond c1) ++ "&&" ++ (compileCond c2) ++ ")"
    | otherwise  = "(" ++ (compileCond c1) ++ "||" ++ (compileCond c2) ++ ")"


compileInstr :: Instruction -> String
compileInstr (Assign c e) = (compileCell c) ++ "=" ++ (compileExpr e) ++ ";\n"
compileInstr (If     c i) = "if(" ++ (compileCond c) ++ ")" ++ (compileInstr i)
compileInstr (Point  s i) = s ++ ":  " ++ (compileInstr i)
compileInstr (Goto     s) = "goto " ++ s ++ ";\n"
compileInstr (Input    c) = "scanf(\"%i\",&"  ++ (compileCell c) ++ ");\n"
compileInstr (Output   e) = "printf(\"%i \"," ++ (compileExpr e) ++ ");\n"
compileInstr Halt         = "{free(kov_arr); free(arr); return 0;\n"


compileProgram :: Either ParseError Program -> Either ParseError String
compileProgram (Left err) = (Left err)
compileProgram (Right (Main x)) = Right (beg ++ (foldr f "" x) ++ end)
  where
    f = ((++) . (("    " ++) . compileInstr))
    beg = includes ++ memorymanage ++ start


parse :: String -> Either ParseError Program
parse = (run parseProgram) . prepareForParsing

compile :: String -> Either ParseError String
compile = compileProgram . parse


----------------------------------------------------------------------------------------------------------
----------------------------------  Main
----------------------------------------------------------------------------------------------------------


when :: Monad m => Bool -> m () -> m ()
when cond action = if cond then action else return ()

unless :: Monad m => Bool -> m () -> m ()
unless = when . not


handleArgs :: [String] -> IO()
handleArgs [] = die "No input file.\nRun with --help for guide."
handleArgs xs
    | (elem "--help" xs) = do putStrLn "TODO"
    | otherwise          = compileFile xs


compileFile :: [String] -> IO()
compileFile []          = return ()
compileFile [from]      = compileFile' from (from ++ ".c")
compileFile (from:to:_) = compileFile' from to


compileFile' :: String -> String -> IO()
compileFile' from to = do
        err1 <- doesFileExist from
        unless err1 $ do
            die ("File " <> from <> " does not exist.")
        content <- readFile from
        compileFile'' to (compile content)

compileFile'' :: String -> (Either ParseError String) -> IO()
compileFile'' _  (Left err)  = die ( show err )
compileFile'' to (Right str) = do 
    h <- openFile to WriteMode
    hPutStrLn h str
    hClose h
    
    

main :: IO ()
main = do 
    args <- getArgs
    handleArgs args


