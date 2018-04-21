{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module : ExprDiff
Description : Partially Differentiates, Simplifies and Evaluates an expression in the Expr datatype format
Copyright : (c) Alice Ip @2018
License : WTFPL
Maintainer : ipa1@mcmaster.ca
Stability : experimental
Portability : POSIX
-}

module ExprDiff where

import           ExprType
import           ExprPretty
import qualified Data.Map.Strict as Map

class DiffExpr a where
 
  eval :: Map.Map String a -> Expr a -> a  -- ^ eval takes a list to be mapped from, of type a, and some expression of type a and returns a value of type a
  simplify :: Map.Map String a -> Expr a -> Expr a -- ^ simplify takes a list to be mapped from , and some expression and returns some expression 
  partDiff :: String -> Expr a -> Expr a -- ^ partdiff takes a string holding what to partially differentiate with respect to, an expression and returns a expression

  (!+) :: Expr a -> Expr a -> Expr a -- ^ (!+) takes two expressions and adds it together
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
 
  (!*) :: Expr a -> Expr a -> Expr a -- ^ (!*) takes two expressions and multiplies it together
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2

  val :: a -> Expr a -- ^ val takes the constant in the expression and wraps it with Const
  val x = Const x
 
  var :: String -> Expr a -- ^ var takes the intended variable in the expression and wraps it with Var
  var x = Var x

-- * Expression Evaluation

{-|
These are the functions in the datatype DiffExpr.
Sin,cos and log are predefined functions in prelude. e1 and e2 hold expressions so in each operation
you must evaluate the items in the operation further.
-}

instance (Floating a,Integral a) => DiffExpr a where
  {-| evaluating the expression with the corresponding operators  -}
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Const x) = x
  eval vrs (Sin e1) = sin (eval vrs e1)
  eval vrs (Cos e1) = cos (eval vrs e1)
  eval vrs (Log e1 e2) = logBase e1 (eval vrs e2)
  eval vrs (Exp e1 e2) = (eval vrs e1) ^ (eval vrs e2)
  eval vrs (Ln e1) = log (eval vrs e1)
  eval vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> v
                       Nothing -> error "failed lookup in eval"

-- * Expression Simplification

  {-| simplification of expression using properties of each function -}
  simplify vrs (Const a) = Const a
  simplify vrs (Var a) = case Map.lookup a vrs of
                          Just x -> (Const x)
                          Nothing -> (Var a)

  simplify vrs (Add e1 (Const 0)) = simplify vrs e1
  simplify vrs (Add (Const 0) e1) = simplify vrs e1
  simplify vrs (Add (Const a) (Const b)) = Const(eval vrs (Add (Const a) (Const b)))

  -- | either or both variables in dictionary, addition
  simplify vrs (Add (Var x) (Var y)) = case Map.lookup x vrs of 
        Just a -> case Map.lookup y vrs of             
              Just b -> Const (eval vrs (Add (Const a) (Const b))) -- x present, y present
              Nothing -> Add (Const a) (Var y)                     -- x present, y not present 
        Nothing -> case Map.lookup y vrs of             
              Just b -> Add (Var x) (Const b)                      -- x not present, y present
              Nothing -> Add (Var x) (Var y)                       -- x not present, y not present
 --  |  one variable in dictionary, addition
  simplify vrs (Add (Var x) (Const a)) = case Map.lookup x vrs of
        Just b -> Const(eval vrs (Add(Const a) (Const b)))  
        Nothing -> Add (Var x) (Const a)
--  | one variable in dictionary, addition, other order                              
  simplify vrs (Add (Const a) (Var x)) = case Map.lookup x vrs of 
        Just b -> Const(eval vrs (Add(Const a) (Const b)))            
        Nothing -> Add (Var x) (Const a)  

  simplify vrs (Add e1 e2) = Add (simplify vrs e1) (simplify vrs e2)

  simplify vrs (Mult e1 (Const 1)) = simplify vrs e1
  simplify vrs (Mult (Const 1) e1) = simplify vrs e1
  simplify vrs (Mult (Const 0) _ ) = (Const 0)
  simplify vrs (Mult _ (Const 0)) = (Const 0)
  simplify vrs (Mult (Const a) (Const b)) = Const (eval vrs (Mult (Const a)(Const b)))
-- | either or both variables in dictionary, multiplication
  simplify vrs (Mult (Var x) (Var y)) = case Map.lookup x vrs of
        Just a -> case Map.lookup y vrs of             
              Just b -> Const (eval vrs (Mult (Const a) (Const b))) -- x present, y present
              Nothing -> Mult (Const a) (Var y)                     -- x present, y not present 
        Nothing -> case Map.lookup y vrs of             
              Just b -> Mult (Var x) (Const b)                      -- x not present, y present
              Nothing -> Mult (Var x) (Var y)                       -- x not present, y not present

-- | one variable being in dictionary, multiplication
  simplify vrs (Mult (Var x) (Const a))= case Map.lookup x vrs of 
        Just b -> Const(eval vrs (Mult (Const a) (Const b)))  
        Nothing -> Mult (Var x) (Const a)

-- | one variable being in dictionary, multiplication, other order
  simplify vrs (Mult (Const a )(Var x))= case Map.lookup x vrs of 
        Just b -> Const(eval vrs (Mult (Const a) (Const b)))  
        Nothing -> Mult (Var x) (Const a)
  simplify vrs (Mult e1 e2 ) = Mult (simplify vrs e1) (simplify vrs e2)   -- multiplication base case 

-- **  Sin and Cos Simplification

{- Performs the following functions:
   1. Evaluates constant right away
   2. Checks if any variables present, and evaluates if so, or else stays the same
   3. Simplifies the expressions inside if possible
-}

  simplify vrs (Sin (Const a)) = Const(eval vrs (Sin (Const a))) -- 1. Constant
  simplify vrs (Sin (Var x)) = case Map.lookup x vrs of          -- 2. Variable
        Just b -> Const(eval vrs (Sin (Const b)))                  
        Nothing -> Sin (Var x)
  simplify vrs (Sin e1) = Sin (simplify vrs e1)                  -- 3. Base Case

  simplify vrs (Cos (Const a)) = Const(eval vrs (Cos (Const a))) -- 1. Constant
  simplify vrs (Cos (Var x)) = case Map.lookup x vrs of          -- 2. Variable
        Just b -> Const(eval vrs (Cos (Const b)))
        Nothing -> Cos (Var x)
  simplify vrs (Cos e1) = Cos (simplify vrs e1)                  -- 3. Base Case

-- **  Log Simplification
{- Performs the following functions:
   1. Checks for simplification using log rules
      a. Log of 1, regardless of base is 0
   2. Evaluates if both are constants right away
   3. Checks if any variables present, and evaluates if so, or else stays the same
  4. Simplifies the expression inside if possible
-}

  simplify vrs (Log _ (Const 1)) = (Const 0) -- 1.a
  simplify vrs (Log e1 (Const b)) = Const(eval vrs (Log e1 (Const b))) -- 2. Constant

-- |  3.one variable in dictionary, log,
  simplify vrs (Log e1(Var x))= case Map.lookup x vrs of 
        Just b -> Const(eval vrs (Log e1 (Const b)))  
        Nothing -> Log e1 (Var x)

  simplify vrs (Log e1 e2) = Log e1 (simplify vrs e2)  -- 4.base case

-- **  Exponent Simplification
  simplify vrs (Exp (Const(0)) e1) = Const(0)
  simplify vrs (Exp (Const(1)) e1) = Const(1)
  simplify vrs (Exp e1 (Const(1))) = e1
  simplify vrs (Exp e1 (Const(0))) = Const(1)
  simplify vrs (Exp (Exp e1 e2) e3) = Exp e1 (Add e2 e3)

-- | either or both variables in dictionary, Expr
  simplify vrs (Exp (Var x) (Var y)) = case Map.lookup x vrs of 
          Just a -> case Map.lookup y vrs of
              Just b -> Const(eval vrs (Exp (Const a) (Const b))) 
              Nothing -> Exp (Const a) (Var y)                      
          Nothing -> case Map.lookup y vrs of
              Just b -> Exp (Var x) (Const b)
              Nothing -> Exp (Var x) (Var y)

-- | one variable in dictionary, Expr
  simplify vrs (Exp (Const a) (Var x)) = case Map.lookup x vrs of
          Just b -> Const (eval vrs (Exp (Const a) (Const b)))
          Nothing -> Exp (Const a) (Var x) 

-- | one variable in dictionary, Expr, other order
  simplify vrs (Exp (Var x) (Const a)) = case Map.lookup x vrs of
          Just b -> Const(eval vrs (Exp (Const b) (Const a)))
          Nothing -> Exp (Var x) (Const a)  
  simplify vrs (Exp e1 e2) = Exp (simplify vrs e1) (simplify vrs e2)    

-- * Partial Differentiation 
  partDiff x (Const a) = Const (0)                                                 --  ^ constant derivative is 0
  partDiff x (Var v) = if v == x then Const 1 else Var v                           --  ^ if variable is the right one, derive, if not keep
  partDiff x (Add e1 e2) = Add (partDiff x e1) (partDiff x e2)                     --  ^ derivative in addition 
  
  partDiff x (Ln e1) = Mult (Exp e1 (Const(-1))) (partDiff x e1)
  partDiff x (Log e1 e2) = Mult (Exp (Mult e2 (Ln (Const e1)))(Const (-1))) (partDiff x e2)

  partDiff x (Sin e1) = Mult (Cos e1) (partDiff x e1)
  partDiff x (Cos e1) = Mult (Mult (Sin e1) (Const (-1))) (partDiff x e1)

  partDiff x (Mult (Const a) (Var v)) = Const a
  partDiff x (Mult (Var v) (Const a)) = Const a
  partDiff x (Mult e1 e2) = Add (Mult (partDiff x e1) e2) (Mult e1 (partDiff x e2))
  
  --  / Expression to the power of a constant, e.g (x+3)^3
  partDiff x (Exp e1 (Const a)) = Mult (Mult (Const a) (Exp e1 (Add (Const a ) (Const (-1))))) (partDiff x e1)

    --  / Constant to the power of an expression, e.g 2^(2*x+3)
  partDiff x (Exp (Const a) e1) = Mult (Mult (Exp (Const a) e1) (Ln (Const a))) (partDiff x e1) 

    --  / Expression to the power of a expression
  partDiff x (Exp e1 e2) = Mult (Mult(Mult e2 (Exp e1 (Add  (Const (-1))  e2))) (partDiff x e2)) (partDiff x e1)
