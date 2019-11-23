module Debug where


import System.IO.Unsafe


constPrint :: String -> a -> a
constPrint s x = const x $! (unsafePerformIO $! putStrLn s)

infixr 0 ?
(?) :: Show a => a -> b -> b
x ? y = constPrint (show x) y
