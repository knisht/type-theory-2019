module Reduction where
import Grammar as G
import Parser
import Lexer
import qualified Data.Set as Set
import Control.Monad.State.Strict
import Data.Bifunctor
import qualified Data.IntMap as Map

data MExpr = Anchor Int 
           | MLambda String MExpr
           | MAppl MExpr MExpr
           | MVar String
           deriving (Eq, Ord)

type MMap = Map.IntMap MExpr
type MState = State (MMap, Int)
type MSet = Set.Set String

rehydrate :: G.Expr -> MExpr
rehydrate e = case e of
  (Lambda s e) -> 
    MLambda s (rehydrate e) 
  (Appl a b)   -> 
    MAppl (rehydrate a) (rehydrate b)
  (Var s)      -> MVar s


createAnchor :: MExpr -> MState MExpr
createAnchor e = case e of
  (Anchor _) -> return e
  _          -> do 
    i <- snd <$> get 
    let a = Anchor i
    modify (bimap (Map.insert i e) (+ 1))
    return a

getM :: String -> MExpr
getM s = rehydrate $ fromRight undefined $ parseLambda (alexScanTokens s)


doSubstitution :: MExpr -> String -> MSet -> MExpr -> MState MExpr
doSubstitution left var contextVars target = do
  vars <- freeVars target
  let nvars = Set.delete var vars
  doSubstitutionImpl left var target (Set.union vars contextVars) Set.empty

removeCollisions :: MSet -> MSet -> MExpr -> MState (MExpr, Bool)
removeCollisions set met e = if (Set.null met) then return (e, False) else case e of
  m@(Anchor i) -> do
    expr <- getMExpr m
    (newExpr, res)  <- removeCollisions set met expr 
    modify $ first (Map.insert i newExpr)
    return (m, res)
  (MAppl e1 e2) -> do
    (left, res1)  <- removeCollisions set met e1 
    (right, res2) <- removeCollisions set met e2 
    return $ (MAppl left right, res1 || res2) 
  (MVar v) -> let nv = findEncoded set met v in
    return $ (MVar nv, not (v == nv)) 
  (MLambda s e1) -> do
    if s `Set.member` met then return (e, False) else do
      let newS = findEncoded set met s
      (nexpr, res) <- removeCollisions set met e1 
      return (MLambda newS nexpr, res || not (s == newS))
     
findEncoded :: MSet -> MSet -> String -> String
findEncoded set met s = if (s `Set.member` set) && (s `Set.member` met) then findEncodedImpl set 0 s else s

findEncodedImpl :: MSet -> Int -> String -> String
findEncodedImpl set ind name = let newS = name ++ (show ind) in
  if not (newS `Set.member` set) then newS else findEncodedImpl set (ind + 1) name

doSubstitutionImpl :: MExpr -> String -> MExpr -> MSet -> MSet -> MState MExpr
doSubstitutionImpl left var target fvars met = do
  let runSubst x y = doSubstitutionImpl x var target fvars y
  inside <- unwrapTillNearest left >>= getMExpr
  case inside of
    l@(MLambda s e) -> do
      if s == var 
      then do
        (expr, res) <- removeCollisions fvars met l
        return (if res then expr else left) 
      else do
        let newMet = (Set.insert s met)
        let newS = findEncoded fvars newMet s
        expr <- runSubst e newMet
        return $ MLambda newS expr
    (MAppl e1 e2) -> do
                     expr1 <- runSubst e1 met
                     expr2 <- runSubst e2 met
                     return $ MAppl expr1 expr2
    v@(MVar s) -> let newS = findEncoded fvars met s 
                      nvar = MVar newS               in
      return $ if var == s then target else nvar


freeVars :: MExpr -> MState MSet
freeVars e = case e of
  (MAppl e1 e2)  -> do
    left  <- freeVars e1
    right <- freeVars e2
    return $ Set.union left right
  (MLambda s e1) -> freeVars e1 >>= return . Set.delete s
  (Anchor i)     -> (freeVars <=< getMExpr) e
  (MVar v)       -> return (Set.singleton v)


getMExpr :: MExpr -> MState MExpr
getMExpr m = case m of
  (Anchor i) -> get >>= (return . fst) >>= (return . (flip (Map.!) i))
  m -> return m



findReduction :: MExpr -> MSet -> MState (MExpr, Bool)
findReduction e cvars = do
  case e of
    (MAppl _ _)   -> reduceAppl e cvars
    (MLambda s e) -> (return e) >>= (\e -> tryReduce e (Set.insert s cvars) (MLambda s))
    m@(MVar x)    -> return (m, False)
    m@(Anchor s)  -> do 
                     expr       <- unwrapTillNearest m >>= getMExpr 
                     (red, suc) <- findReduction expr cvars
                     if suc then do
                               modify $ first (Map.insert s red) 
                               return (m, True)
                     else do 
                            return (m, False) 

reduceAppl :: MExpr -> MSet -> MState (MExpr, Bool)
reduceAppl gm@(MAppl e1 e2) cvars = do
    ne1 <- unwrapTillNearest e1
    forceReduceAppl (MAppl ne1 e2) cvars

unwrapTillNearest :: MExpr -> MState MExpr
unwrapTillNearest e = do
  case e of 
    (Anchor _) -> do
      expr <- getMExpr e
      case expr of
        (Anchor _) -> unwrapTillNearest expr
        _ -> return e 
    other      -> return other

forceReduceAppl :: MExpr -> MSet -> MState (MExpr, Bool)
forceReduceAppl gm@(MAppl e1 e2) cvars = do 
  ne1 <- getMExpr e1
  case ne1 of
    (MLambda s innerE) -> do
      newAnchor <- createAnchor e2
      expr      <- doSubstitution innerE s cvars newAnchor
      return (expr, True)
    other -> do  
      m1 <- tryReduce e1 cvars (\x -> MAppl x e2)
      if (snd m1) then return m1 
                  else tryReduce e2 cvars (\x -> MAppl other x) 


tryReduce :: MExpr -> MSet -> (MExpr -> MExpr) -> MState (MExpr, Bool)
tryReduce x cvars f = do
  (r, b) <- findReduction x cvars
  return $ if b then (f r, True) else (f x, False)


reduceExpr :: Int -> Int -> Expr -> [String]
reduceExpr i k e = let initial = (Map.empty, 0) in
  evalState (iteratedReduction k i (rehydrate e)) initial 


iteratedReduction :: Int -> Int -> MExpr -> MState [String]
iteratedReduction step iters initial = iteratedReductionHelper step iters 0 initial 


iteratedReductionHelper :: Int -> Int -> Int -> MExpr -> MState [String]
iteratedReductionHelper step iters counter initial = 
  if iters == 0
  then (dehydrate initial) >>= return . (:[])
  else do
    expr               <- if counter `mod` step == 0 then dehydrate initial 
                                                     else return $ ""
    (e, managedReduce) <- (findReduction initial) Set.empty
    let leftIters = if managedReduce then iters - 1 else 0 
    list               <- iteratedReductionHelper step leftIters (counter + 1) e
    if counter `mod` step == 0 && managedReduce then return $ expr : list else return list


dehydrate :: MExpr -> MState String 
dehydrate e =
  case e of
    (MAppl e1 e2) -> do 
      r1 <- dehydrate e1
      r2 <- dehydrate e2
      return $ "(" ++ r1 ++ " " ++ r2 ++ ")"
    (MLambda s e) -> do
      r <- dehydrate e
      return $ "(\\" ++ s ++ "."  ++ r ++ ")"
    (MVar s)      -> return $ s
    m@(Anchor i)  -> (dehydrate <=< getMExpr) m


fromRight :: a -> Either b a -> a
fromRight x (Left _)  = x
fromRight _ (Right x) = x 


