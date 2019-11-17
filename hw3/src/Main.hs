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
  let numbers = (map read . words) line :: [Int] 
  if length numbers < 2 
    then putStrLn "no" 
    else do
      let m = (numbers !! 0)
      let k = (numbers !! 1)
      input <- getLine
      --let result = reduceLambda m k input
      putStr ""


runE :: String -> Expr
runE = fromRight . parseLambda . alexScanTokens

eqs :: String -> [Equation]
eqs = fst . genSystemOfTypeEq . runE
