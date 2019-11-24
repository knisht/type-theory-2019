module Grammar where

import Data.List (intercalate)

data Expr = Lambda String Expr  |
            Appl Expr Expr    | 
            Var String        
            deriving (Eq, Ord) 


instance Show Expr where
  show (Var x) = x
  show (Lambda e1 e2) = "(\\" ++ e1 ++ ". " ++ show e2 ++ ")"
  show (Appl e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"

