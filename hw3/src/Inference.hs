module Inference where
import Grammar
import Data.Set
import Control.Monad.State.Strict
import Data.Map.Strict as Map
import Data.IntMap as IMap
import Data.Set as Set
import Data.Bifunctor
import Data.List as List
import Unification
import Types
import Debug

data Term = Term Expr SType
type MState a = State (IMap.IntMap SType, Map String Int, Int) a 




infer :: Expr -> String
infer e = let (varmap, exprs, mytype) = genSystemOfTypeEqFull e
              freeVars = genFreeVars e
              mmap = unify exprs in
 case mmap of
    Nothing  -> "Expression has no type\n"
    Just map -> let hypset = genHypSet map varmap freeVars in
      (snd $ (evalState (genProofs 0 e hypset) (map, Map.empty, 1))) >>= (++ "\n")

genSystemOfTypeEq :: Expr -> ([Equation], SType)
genSystemOfTypeEq e = evalState (genSystemOfTypeEqS e) (Map.empty, 1)

genSystemOfTypeEqFull :: Expr -> (Map String Int, [Equation], SType)
genSystemOfTypeEqFull e = case runState (genSystemOfTypeEqS e) (Map.empty, 1) of
  ((eqs, stype), (map, _))  -> (map, eqs, stype)

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
    ungenAtom s
    return $ (eqs, atom :-> te)


genFreeVars :: Expr -> Set String
genFreeVars e = case e of
  Var s -> Set.singleton s
  Appl a b -> Set.union (genFreeVars a) (genFreeVars b)
  Lambda s e -> Set.delete s (genFreeVars e)

safeGet :: Map String Int -> String -> Int
safeGet m s = case m Map.!? s of
  Just i -> i
  Nothing -> 1

genHypSet :: IMap.IntMap SType -> Map String Int -> Set String -> String
genHypSet imap vmap s =  
  let lambda = (\x -> x ++ " : " ++ (show . (genTypeRaw imap) . (safeGet vmap)) x) 
      row = List.intercalate ", " (Set.elems $ Set.map lambda s)  in
  row 

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

ungenAtom :: String -> State (Map String Int, Int) () 
ungenAtom s = do
  let morphMap = if s == "_" then id else Map.delete s
  modify $ bimap morphMap id

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


ungenAtom3 :: String -> MState ()
ungenAtom3 s = do
  let morphMap = if s == "_" then id else Map.delete s
  modify $ bimap morphMap id

genType :: SType -> MState SType
genType (Atom i) = do
  (map, _, _) <- get
  let result = map IMap.!? i
  case result of
    (Just a) -> return a
    Nothing  -> return $ Atom i

genTypeRaw :: IMap.IntMap SType -> Int -> SType
genTypeRaw map i = let result = map IMap.!? i in
  case result of 
    (Just a) -> a
    Nothing  -> Atom i 


genProofs :: Int -> Expr -> String -> MState (String, [String])
genProofs depth e hyps = case e of 
  (Var s) -> do
    atom <- genAtom3 s
    myType <- genType atom
    let strType = show myType
    return (strType, [(genDepth depth ++ addSpace hyps ++ "|- " ++ render e myType ++ " [rule #1]")])
  (Appl e1 e2) -> do
    (_, left)  <- genProofs (depth + 1) e1 hyps
    (_, right) <- genProofs (depth + 1) e2 hyps
    atom <- genAtom3 "_"
    myType <- genType atom
    let strType = show myType
    return $ (strType, 
             (genDepth depth ++ addSpace hyps ++ "|- " ++ render e myType ++ " [rule #2]") : (left ++ right)) 
  (Lambda s m) -> do
    atom <- genAtom3 s
    myType <- genType atom
    (innerType, innerRepr) <- genProofs (depth + 1) m (addHyp hyps s myType)
    let wholeType = "(" ++ show myType ++ " -> " ++ innerType ++ ")"
    let wholeExprRepr = show e ++ " : " ++ wholeType
    ungenAtom3 s
    return (wholeType, (genDepth depth ++ addSpace hyps ++ "|- " ++ wholeExprRepr ++ " [rule #3]") : innerRepr)

addSpace :: String -> String
addSpace [] = []
addSpace s  = s ++ " "

addHyp :: String -> String -> SType -> String
addHyp hyps s stype = let repr = render (Var s) stype in
  case hyps of
  [] -> repr
  _  -> hyps ++ ", " ++ repr

render :: Expr -> SType -> String
render e s = show e ++ " : " ++ show s 

genDepth :: Int -> String
genDepth 0 = ""
genDepth i = genDepth (i - 1) ++ "*   "


hardExpr = "((\\s.(s ((s s) s))) (\\p.(\\g.(\\m.(\\f.f)))))"
