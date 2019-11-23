module Main where

import Grammar (Expr (..))
import Lexer (alexScanTokens)
import Parser (parseLambda)
import Inference
import Unification
import Types
import Data.List

main :: IO ()
main = do
  line <- getLine
  let expr = runE line
  let proof = infer expr
  putStr proof


runE :: String -> Expr
runE = fromRight . parseLambda . alexScanTokens

eqs :: String -> [Equation]
eqs = fst . genSystemOfTypeEq . runE
