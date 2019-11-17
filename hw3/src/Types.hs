module Types where 

data SType = Atom Int
           | SType :-> SType
           deriving (Eq)

data Equation = SType := SType

instance Show SType where
  show t = case t of
    Atom i -> "t" ++ show i
    l :-> r -> "(" ++ show l ++ " -> " ++ show r ++ ")"
 

instance Show Equation where
  show (a := b) = show a ++ " = " ++ show b
