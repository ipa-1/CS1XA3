{-|
Module : ExprTest
Description : Used to make test cases to test if the datatype works as expected
Copyright : (c) Alice Ip @2018
License : WTFPL
Maintainer : ipa1@mcmaster.ca
Stability : experimental
Portability : POSIX
-}

-- http://5outh.blogspot.ca/2013/05/symbolic-calculus-in-haskell.html

module ExprTest where

import           ExprDiff
import           ExprParser
import           ExprPretty
import           ExprType

import qualified Data.Map.Strict as Map
import           Test.QuickCheck

{- This module defines different tests to check if the Expr datatype works -}
	
-- | test for the Int Expr
sampleExpr1 :: Expr Int
sampleExpr1 = (var "x") !+ (var "y")

-- | test for Double Expr
listToExpr1 :: [Double] -> Expr Double
listToExpr1 [x]    = Const x
listToExpr1 (x:xs) = Add (Const x) (listToExpr1 xs)
listToExpr1 []     = error "Not list to expression for empty"

-- | general test
test1 :: Int -> Bool
test1 x = eval (Map.fromList [("x",x),("y",-x)]) sampleExpr1 == 0