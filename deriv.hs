import System.Environment
import System.IO (stdout, stderr, hPutStr, hPutStrLn)
import Data.Char (digitToInt, isDigit, isLetter)

-- 1. Grammar
-- <expression> ::= <signed-term> | <expression> "+" <term> | <expression> "-" <term>
-- <signed-term> ::= "-" <term> | <term>
-- <term> ::= <factor> | <term> * <factor>
-- <factor> ::= <element> | <element> ** <numeral>
-- <element> ::= <variable> | <numeral> | "(" <expression> ")"
-- <variable> ::= [a-z]
-- <numeral> ::= [0-9]+

-- 2. Info Given
data ME = Num Int 
        | Var Char
        | Group ME
        | Add ME ME
        | Sub ME ME
        | Mul ME ME
        | Power ME Int
        | Neg ME
        deriving (Show, Ord, Eq)

-- 3. Parsing
parseME :: [Char] -> Maybe ME
parseME s =
  case parseExp s of
    Just (e, []) -> Just e
    _ -> Nothing

parseExp :: [Char] -> Maybe (ME, [Char])
parseExp s =
  case parseSignedTerm s of
    Just (r, more) -> extendME(r, more)
    _ -> Nothing

extendME :: (ME, [Char]) -> Maybe (ME, [Char])
extendME (e1, []) = Just (e1, [])
extendME (e1, ('+':after_plus)) =
  case parseTerm(after_plus) of
    Just(e2, more) -> extendME(Add e1 e2, more)
    _ -> Nothing
extendME (e1, ('-':after_minus)) =
  case parseTerm(after_minus) of
    Just(e2, more) -> extendME(Sub e1 e2, more)
    _ -> Nothing
extendME (e1, c:more) = Just (e1, c:more)

parseSignedTerm :: [Char] -> Maybe (ME, [Char])
parseSignedTerm ('-':after_minus) =
  case parseTerm after_minus of
    Just(e1, more) -> Just(Neg e1, more)
    _ -> Nothing
parseSignedTerm s = parseTerm s

parseTerm :: [Char] -> Maybe (ME, [Char])
parseTerm s =
  case parseFactor s of
    Just(e1, more) -> extendTerm(e1, more)
    _ -> Nothing

extendTerm :: (ME, [Char]) -> Maybe (ME, [Char])
extendTerm (e1, []) = Just (e1, [])
extendTerm (e1, ('*':after_asterisk)) =
  case parseFactor(after_asterisk) of
    Just(e2, more) -> extendTerm(Mul e1 e2, more)
    _ -> Nothing    
extendTerm (e1, c:more) = Just (e1, c:more)

parseFactor :: [Char] -> Maybe (ME, [Char])
parseFactor s =
  case parseElement s of
    Just(e1, more) -> extendFactor(e1, more)
    _ -> Nothing

extendFactor :: (ME, [Char]) -> Maybe (ME, [Char])
extendFactor (e1, []) = Just (e1, [])
extendFactor (e1, ('*':'*':after_asterisk)) =
  case numeral(after_asterisk) of
    Just(num, more) -> Just((Power e1 num), more)
    _ -> Nothing    
extendFactor (e1, c:more) = Just (e1, c:more)

parseElement :: [Char] -> Maybe (ME, [Char])
parseElement ('(':more) =
  case parseExp(more) of
    Just (re, ')':yet_more) -> Just(Group re, yet_more)
    _ -> Nothing
parseElement s =
  case isLetter (head s) of
    True -> parseVariable (s)
    _ -> parseNumeral (s)

numeral :: [Char] -> Maybe (Int, [Char])
numeral [] = Nothing
numeral (num:[]) = Just(digitToInt num, [])
numeral num =
  case reads num :: [(Int, [Char])] of
    [(n, s)] -> Just(n, s)
    _ -> Nothing

parseNumeral :: [Char] -> Maybe (ME, [Char])
parseNumeral s =
  case numeral s of
    Just(n, more) -> Just(Num n, more)
    _ -> Nothing

parseVariable :: [Char] -> Maybe (ME, [Char])
parseVariable [] = Nothing
parseVariable (x:xs) =
  case isLetter x of
    True -> Just(Var x, xs)

-- 4. Unparsing
unparseME :: ME -> [Char]
unparseME (Add me1 me2) = (unparseME me1) ++ "+" ++ (unparseME me2)
unparseME (Sub me1 me2) = (unparseME me1) ++ "-" ++ (unparseME me2)
unparseME (Mul me1 me2) = (unparseME me1) ++ "*" ++ (unparseME me2)
unparseME (Power me1 n) = (unparseME me1) ++ "**" ++ (show n)
unparseME (Neg me1) = "-" ++ (unparseME me1)
unparseME (Group me1) = '(': (unparseME me1) ++ ")"
unparseME (Var c) = c:[]
unparseME (Num n) = (show n)

-- 5. Differentiation Problem
deriv :: ME -> Char -> ME
deriv (Num _) _ = Num 0
deriv (Var c) var =
  case (c == var) of
    True -> Num 1
    _ -> Num 0
deriv (Neg me) var = Neg(Group(deriv me var))
deriv (Add me1 me2) var = Add (deriv me1 var) (deriv me2 var)
deriv (Sub me1 me2) var = Sub (deriv me1 var) (deriv me2 var)
deriv (Mul me1 me2) var = Add (Mul me2 (deriv me1 var)) (Mul me1 (deriv me2 var))
deriv (Group me1) var = Group (deriv me1 var)
deriv (Power me1 n) var = Mul (Num n) (Power me1 (n-1))

-- 6. Simplifying
simplifyME :: ME -> ME
simplifyME (Var c) = makeVar c
simplifyME (Num n) = makeNum n
simplifyME (Neg me1) = makeNeg me1
simplifyME (Group me1) = makeGroup (simplifyME me1)
simplifyME (Add me1 me2) = makeAdd (simplifyME me1) (simplifyME me2)
simplifyME (Sub me1 me2) = makeSub (simplifyME me1) (simplifyME me2)
simplifyME (Mul me1 me2) = makeMul (simplifyME me1) (simplifyME me2)
simplifyME (Power me1 n) = makePower (simplifyME me1) n

makePower :: ME -> Int -> ME
makePower me1 0 = Num 1
makePower me1 1 = me1
makePower (Num a) b = Num (a^b)
makePower me1 n = Power me1 n

makeMul :: ME -> ME -> ME
makeMul (Num 0) me1 = Num 0
makeMul (Num 1) me1 = me1
makeMul (Num a) (Num b) = Num (a*b)
makeMul me1 (Num a) = makeMul (Num a) me1
makeMul me1 (Mul me2 me3) = makeMul (makeMul me1 me2) me3
makeMul (Power me1 a) (Power me2 b)
  | me1 == me2 = (makePower me1 (a+b))
  | otherwise = Mul (makePower me1 a) (makePower me2 b)
makeMul me1 me2 = Mul me1 me2

makeAdd :: ME -> ME -> ME
makeAdd me1 (Num 0) = me1
makeAdd (Num a) (Num b) = Num (a+b)
makeAdd (Add me1 (Num a)) (Num b) = Add me1 (Num(a+b))
makeAdd (Add me1 (Num a)) me2 = makeAdd (Add me1 me2) (Num a)
makeAdd (Num a) me1 = makeAdd me1 (Num a)
makeAdd (Mul (Num a) me1) (Mul (Num b) me2)
  | me1 == me2 = (makeMul (Num (a+b)) me1)
  | otherwise = Add (makeMul (Num a) me1) (makeMul (Num b) me2)
makeAdd me1 me2 = Add me1 me2

makeSub :: ME -> ME -> ME
makeSub (Num 0) me1 = makeNeg me1
makeSub me1 (Num 0) = me1
makeSub (Num a) (Num b) = Num (a+b)
makeSub (Num a) me1 = makeAdd (makeNeg me1) (Num a)
makeSub (Sub me1 (Num a)) (Num b) = Sub me1 (Num(a+b))
makeSub me1 me2 = Sub me1 me2

makeNeg :: ME -> ME
makeNeg me1 = Neg me1

makeGroup :: ME -> ME
makeGroup me1 = me1

makeVar :: Char -> ME
makeVar c = Var c

makeNum :: Int -> ME
makeNum n = Num n

-- 7.  Command line interface
main = do
  [equation, variable:rest] <- getArgs
  case parseME equation of
    Just me -> print (unparseME(simplifyME(deriv (simplifyME(me)) variable)))
    _ -> hPutStr stdout "Could not parse\n"
