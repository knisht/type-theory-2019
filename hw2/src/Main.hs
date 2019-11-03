module Main where

import Grammar (Expr (..))
import Lexer (alexScanTokens)
import Parser (parseLambda)
import Reduction
import Data.Either
import Data.List.Split

main :: IO ()
main = do
  line <- getLine
  let numbers = splitOn " " line
  let m = read (numbers !! 0) :: Int
  let n = read (numbers !! 1) :: Int
  input <- getContents
  case parseLambda (alexScanTokens input) of
    Left err   -> putStrLn err
    Right expr -> putStrLn $ show expr


reduceLambda :: Int -> Int -> String -> String
reduceLambda m k s = 
  let expr = fromRight undefined $ parseLambda (alexScanTokens s) 
      results = reduceExpr m k expr  
  in
  (show <$> results) >>= (\line -> line ++ "\n") 

runParser :: String -> Expr
runParser s = fromRight undefined $ parseLambda (alexScanTokens s)
