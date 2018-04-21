{-|
Module : ExprPretty
Description : Shows the expression in a organized format
Copyright : (c) Alice Ip @2018
License : WTFPL
Maintainer : ipa1@mcmaster.ca
Stability : experimental
Portability : POSIX
-}

module ExprPretty where

import           ExprType

-- | wraps each item in the expression in parenthesis
parens :: String -> String
parens ss = "(" ++ ss ++ ")"

-- | defines how each operation is displayed
instance Show a => Show (Expr a) where
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)
  show (Add e1 e2)  = parens (show e1) ++ " !+ " ++ parens (show e2)
  show (Var ss)     = parens $ "var \"" ++ ss ++ "\""
  show (Const x)    = parens $ "val " ++ show x
  show (Log a e2)   = parens $ "log " ++ "base" ++ show a ++ (show e2)
  show (Sin e1)     = parens $ "sin " ++ (show e1)
  show (Cos e1)     = parens $ "cos " ++  (show e1)
  show (Exp e1 e2)  = parens (show e1) ++ " !^ " ++ parens (show e2)
  show (Ln e1)      = parens $ "Ln " ++ show(e1)