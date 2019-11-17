module Inference where
import Grammar
import Data.Set
import Control.Monad.State.Strict
import Data.Map.Strict as Map
import Data.IntMap as IMap
import Data.Bifunctor
import Unification
import Types


data Term = Term Expr SType
type MState a = State (IMap.IntMap SType, Map String Int, Int) a 




infer :: Expr -> String
infer e = let (exprs, mytype) = genSystemOfTypeEq e
              mmap = unify exprs in
 case mmap of
    Nothing  -> "There is no type"
    Just map -> (evalState (genProofs 0 e "") (map, Map.empty, 0)) >>= (++ "\n")

genSystemOfTypeEq :: Expr -> ([Equation], SType)
genSystemOfTypeEq e = evalState (genSystemOfTypeEqS e) (Map.empty, 0)

genSystemOfTypeEqS :: Expr -> State (Map String Int, Int) ([Equation], SType)
genSystemOfTypeEqS e = case e of
  (Var s) -> do
    atom <- genAtom s
    return ([], atom)
  (Appl e1 e2) -> do
    (left,  t1) <- genSystemOfTypeEqS e1
    (right, t2) <- genSystemOfTypeEqS e2
    atom <- genAtom "_"
    return $ ((t1 := (t2 :-> atom)) : (left ++ right), atom)
  (Lambda s e) -> do
    atom <- genAtom s
    (eqs, te) <- genSystemOfTypeEqS e
    return $ (eqs, atom :-> te)



genAtom :: String -> State (Map String Int, Int) SType
genAtom s = do
  (map, i) <- get
  let result = map Map.!? s
  case result of
    (Just j) -> return $ Atom j
    Nothing  -> do  
      let morphMap = if s == "_" then id else Map.insert s i
      modify $ bimap morphMap (+1)
      return $ Atom i


fromRight :: Either a b -> b
fromRight e = case e of
  Left _  -> undefined
  Right x -> x

genAtom3 :: String -> MState SType
genAtom3 s = do
  (_, map, i) <- get
  let result = map Map.!? s
  case result of
    (Just j) -> return $ Atom j
    Nothing  -> do
      let morphMap = if s == "_" then id else Map.insert s i
      modify $ bimap morphMap (+1)
      return $ Atom i


genType :: SType -> MState SType
genType (Atom i) = do
  (map, _, _) <- get
  let result = map IMap.!? i
  case result of
    (Just a) -> return a
    Nothing  -> return $ Atom i

genProofs :: Int -> Expr -> String -> MState [String]
genProofs depth e hyps = case e of 
  (Var s) -> do
    atom <- genAtom3 s
    myType <- genType atom
    return [(genDepth depth ++ hyps ++ "|- " ++ render e myType ++ " [rule #1]")]
  (Appl e1 e2) -> do
    left  <- genProofs (depth + 1) e1 hyps
    right <- genProofs (depth + 1) e2 hyps
    atom <- genAtom3 "_"
    myType <- genType atom
    return $ (genDepth depth ++ hyps ++ "|- " ++ render e myType ++ " [rule #2]") : (left ++ right) 
  (Lambda s m) -> do
    atom <- genAtom3 s
    myType <- genType atom
    let newHyp = render (Var s) myType
    innerRepr <- genProofs (depth + 1) m (newHyp ++ ", " ++ hyps)
    return $ (genDepth depth ++ hyps ++ "|- " ++ render e myType ++ " [rule #3]") : innerRepr

render :: Expr -> SType -> String
render e s = show e ++ " : " ++ show s 

genDepth :: Int -> String
genDepth 0 = ""
genDepth i = genDepth (i - 1) ++ "*   "











