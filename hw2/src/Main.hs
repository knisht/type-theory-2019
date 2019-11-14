module Main where

import Grammar (Expr (..))
import Lexer (alexScanTokens)
import Parser (parseLambda)
import Reduction
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
      let result = reduceLambda m k input
      putStr result


reduceLambda :: Int -> Int -> String -> String
reduceLambda m k s = 
  let tk = if k == 0 then m else k
      expr = fromRight (Var "") $ parseLambda (alexScanTokens s) 
      results = reduceExpr m tk expr  
  in
  (results) >>= (++"\n") 

