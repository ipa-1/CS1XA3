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

{-| the parser for double-}
exprD :: Parser (Expr Double)
exprD = error "define me!" -- #TODO complete parser

{-| the parser for float-}
exprF :: Parser (Expr Float)
exprF = error "define me!" -- #TODO complete parser