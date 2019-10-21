module Main where

import Grammar (Expr (..))
import Lexer (alexScanTokens)
import Parser (parseLambda)

main :: IO ()
main = do
  input <- getContents
  case parseLambda (alexScanTokens input) of
    Left err   -> putStrLn err
    Right expr -> putStrLn $ show expr
