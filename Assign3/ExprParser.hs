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
-- This module parses a string into our datatype

parseExprD :: String -> Expr Double
parseExprD ss = case parse exprD "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

parseExprF :: String -> Expr Float
parseExprF ss = case parse exprF "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

exprD :: Parser (Expr Double)
exprD = error "define me!" -- #TODO complete parser

exprF :: Parser (Expr Float)
exprF = error "define me!" -- #TODO complete parser