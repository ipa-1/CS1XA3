{-|
Module : ExprParser
Description : Converts a String into the datatype format
Copyright : (c) Alice Ip @2018
License : WTFPL
Maintainer : ipa1@mcmaster.ca
Stability : experimental
Portability : POSIX

-}


module ExprParser (parseExprD,parseExprF) where


import           ExprType
import           Text.Parsec
import           Text.Parsec.String

{-| Class Diff Expr
-- This module parses a string into our datatype

Methods:
parseExprD : takes a string and parses it into a Expr of the type Double
paresExprF : takes a string and parses it into a Expr of the type Float
exprD : the parser for Double
exprF : the parser for float
-}



{-| parseExprD parses a String into a Expr of the type Double -}
parseExprD :: String -> Expr Double
parseExprD ss = case parse exprD "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

{-| parseExprD parses a String into a Expr of the type Float -}
parseExprF :: String -> Expr Float
parseExprF ss = case parse exprF "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

{-| the parser for double into our Expr datatype-}
exprD :: Parser (Expr Double)
exprD = let
 brackets
 exponents
 divMult
 AddSub
 in


{-| the parser for float into our Expr datatype-}
exprF :: Parser (Expr Float)
exprF = error "define me!" -- #TODO complete parser

 -- ** parsing operators

 -- | parses constant
pConst :: Parser (Expr a)
pConst = do {c <- integer;
             return (Const c)}

-- | parses variable
pVar :: Parser(Expr a)
pVar = do {v <- string;
           return (Var v)}
           
-- | parses addition
pAdd :: Parser (Expr a -> Expr a -> Expr a)
pAdd = do {symbol "+"; return Add} 

-- | parses multiplication
pMul :: Parser(Expr a -> Expr a -> Expr a)
pMul = do {symbol "*"; return Mult}

-- | parses Ln, Cos, Sin, Exponential
pLn :: Parser(Expr a -> Expr a ->Expr a)
pLn = do {symbol "ln"; return Ln} 
     <|> do {symbol "cos"; return Cos}
     <|> do {symbol "sin"; return Sin}
     <|> do {symbol "^"; return Exp}

-- ** General Parsing Functions

-- | parses chars
item :: Parser Char -- parses any single Char
item = let getItem ss = case ss of 
 	(c:cs) -> Just (c,cs)
 	[] -> Nothing
 in Parser getItem

 -- | parses string
string :: String -> Parser String
string (c:cs) = do char c -- parse first char
                   string cs --recurse on the rest of the string
                   return (c:cs) 
string "" = return ""

-- | parses integers
integer :: Parser String
integer = fmap read $ negDigits <|> digits

-- | parses many numbers
digits :: Parser String
digits = many1 digit

-- | parses negatives
negDigits :: Parser String
negDigits = do { symbol "-";
                 ds <- digits;
                 return ('-':ds) }

-- | parses a symbol
symbol :: String -> Parser String
symbol ss = do { spaces;
 ss' <- string ss;
 spaces;
 return ss' }

-- | parses parenthesis
parens :: Parser a -> Parser a
parens p = do { char "(";
 cs <- p;
 char ")";
 return cs }