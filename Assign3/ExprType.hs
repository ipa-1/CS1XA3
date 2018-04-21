{-|
Module : ExprType
Description : Data declaration for datatype 'Expr'
Copyright : (c) Alice Ip @2018
License : WTFPL
Maintainer : ipa1@mcmaster.ca
Stability : experimental
Portability : POSIX

-}


-- http://5outh.blogspot.ca/2013/05/symbolic-calculus-in-haskell.html

module ExprType where
import           Data.List

{--| Creating a Datatype with constructors that can handle basic math operations  -}
data Expr a = Add (Expr a) (Expr a)  -- ^ standard binary addition, between two (Expr a)
            | Mult (Expr a) (Expr a) -- ^ standard binary multiplication between two (Expr a)
            | Const a                -- ^ wrapper for a constant in the expression, or just to wrap something in the Expr datatype
            | Var String             -- ^ wrapper for variable identifer, in the form of a String
            | Log a (Expr a)  -- ^ wrapper for log (Expr a),(the second constructor) to the base (Expr a), (first constructor)
            | Sin (Expr a)           -- ^ wrapper for sine of (Expr a)
            | Cos (Expr a)           -- ^ wrapper for cos of (Expr a)
            | Exp (Expr a) (Expr a)  -- ^ wrapper for a^b, where a is the first constructor and b is the second constructor
            | Ln (Expr a)            -- ^ wrapper for ln of (Expr a)
         
  deriving Eq

{-| Function getVars
 Takes an Expr a and returns the variables in the expression in a list

 This is done by throwing away the constructor and targetting the expression inside until
 the type constructor ( Var a ) is found. the "a" would be the variable we are looking for.
 The `union` function allows for concatenation (++) without duplications in the list
-}

getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 `union` getVars e2  
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Const _)    = []                            -- constants are not variables and have no variables
getVars (Var ident)  = [ident]                       -- variable has been found
getVars (Log e1 e2)  =  getVars e2
getVars (Sin e1) = getVars e1
getVars (Cos e1) = getVars e1
getVars (Exp e1 e2) = getVars e1 `union` getVars e2
getVars (Ln e1) = getVars e1


