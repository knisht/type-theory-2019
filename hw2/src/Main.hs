module Main where

import Grammar (Expr (..))
import Lexer (alexScanTokens)
import Parser (parseLambda)
import Reduction
import Data.List
import Data.List.Split

main :: IO ()
main = do
  line <- getLine
  let numbers = Data.List.filter (\x -> length x > 0) (splitOn " " line)
  let m = read (numbers !! 0) :: Int
  let k = read (numbers !! 1) :: Int
  input <- getContents
  let result = reduceLambda m k input
  putStr result


reduceLambda :: Int -> Int -> String -> String
reduceLambda m k s = 
  let expr = fromRight undefined $ parseLambda (alexScanTokens s) 
      results = reduceExpr m k expr  
  in
  if (k == 0) then s ++ "\n" else (show <$> results) >>= (++"\n") 


strongReduce :: String -> IO ()
strongReduce s = do
  let reduced = reduceLambda 20 1 s
  putStr reduced
