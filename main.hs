{-# LANGUAGE InstanceSigs #-}

import Data.Char

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
    | Comment String           -- # String
    | Point String Instruction -- String: Instruction
    | Goto String              -- Goto -> String
    | Input Cell               -- Input: Cell
    | Output Cell              -- Output: Cell
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


char :: Char -> Parser Char
char c = satisfy [c] (== c)


space :: Parser Char
space = satisfy "space" isSpace 


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


spaces :: Parser String
spaces = many space


symbol :: String -> Parser String
symbol s = do
    result <- string s
    _ <- spaces
    return result


between :: Parser a -> Parser c -> Parser b -> Parser b
between left right p = do
    _      <- left
    result <- p
    _      <- right
    return result


parens, brackets, braces :: Parser a -> Parser a
parens   p = try (between (symbol "(") (symbol ")") p)
brackets p = try (between (symbol "[") (symbol "]") p)
braces   p = try (between (symbol "{") (symbol "}") p)


sepBy, sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy  p s = sepBy1 p s <|> return []
sepBy1 p s = do
  first <- p
  rest  <- many (s >>= \_ -> p)   -- stručněji jako 'many (s >> p)'
  return (first : rest)



parseListOf :: Parser a -> Parser [a]
parseListOf p = brackets $ p `sepBy` (symbol ",") 


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
    str    <- (many parseChar)
    _      <- string ":" 
    instr  <- parseInstruction
    return (Point str instr)
    where
        parseChar = satisfy "not a quote" (/= ':')

parseGoto :: Parser Instruction 
parseGoto = do 
    _   <- string "Goto"
    str <- many parseAny
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
    cell <- parseCell
    return (Input cell)

parseInstruction :: Parser Instruction 
parseInstruction = choice "Instruction"
    [ try parseAssign
    , try parseIf
    , try parsePoint
    , try parseGoto
    , try parseInput
    , try parseOutput
    ]


----------------------------------------------------------------------------------------------------------
----------------------------------  Parser Program
----------------------------------------------------------------------------------------------------------


parseProgram :: Parser Program 
parseProgram = Main <$> (sepBy parseInstruction (symbol ","))





-----------------------------
includes = "#include <stdio.h>\n#include <stdlib.h>\n\n"
memorymanage = "int get(int idx, int* max, int** arr)\n{if(*max>idx){*arr=(int*)realloc(*arr,sizeof(int)*2*idx;*max=2*idx}return idx;}\n\n"
start = "int main(){\n   int* kov_arr=(int*)malloc(sizeof(int)*26);\n   int max=32;\n   int* arr=(int*)malloc(32*sizeof(int));\n"
end = "   free(kov_arr);\n   free(arr);\n}\n"



fromASCII :: Char -> Int
fromASCII c = (ord c) - 65


translateCell :: Cell -> String
translateCell (Konv    c) = "kov_arr[" ++ (show (fromASCII c)) ++ "]"
translateCell (Dir     n) = "arr[get(" ++ (show n) ++ ", &arr, &max)]"
translateCell (NonDir  c) = "arr[get(" ++ (translateCell c) ++ ", &arr, &max)]"


translateExpr :: Expr -> String
translateExpr (Lit int)   = show int
translateExpr (Var cell)  = translateCell cell
translateExpr (BinOp op e1 e2) = (translateExpr e1) ++ op ++ (translateExpr e2)


translateCond :: Cond -> String
translateCond (CmpOp     op e1 e2) = (translateExpr e1) ++ op ++ (translateExpr e2)
translateCond (LogicalOp op c1 c2) = (translateCond c1) ++ op ++ (translateCond c2)


translateInstr :: Instruction -> String
translateInstr (Assign c e) = (translateCell c) ++ "=" ++ (translateExpr e) ++ ";\n"
translateInstr (If     c i) = "if(" ++ (translateCond c) ++ ")" ++ (translateInstr i)
translateInstr (Comment  s) = "// " ++ s ++ "\n"
translateInstr (Point  s i) =  s ++ (translateInstr i)
translateInstr (Goto     s) =  "goto" ++ s ++ ";\n"
translateInstr (Input    c) =  "scanf(\"%i\",&"  ++ (translateCell c) ++ ");\n"
translateInstr (Output   c) =  "printf(\"%i \",&" ++ (translateCell c) ++ ");\n"


translateProgram :: Either ParseError Program -> Either ParseError String
translateProgram (Right (Main x)) = Right (beg ++ (foldr f "" x) ++ end)
  where
    f = ((++) . (("   " ++) . translateInstr))
    beg = includes ++ memorymanage ++ start
translateProgram (Left err) = (Left err)

printPrg :: Either ParseError String -> String 
printPrg (Left  err) = show err
printPrg (Right prg) = prg


translate :: String -> String
translate = printPrg . translateProgram . (run parseProgram) 

test s = putStr (translate s)


