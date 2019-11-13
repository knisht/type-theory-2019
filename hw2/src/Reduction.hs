module Reduction where
import Grammar as G
import Parser
import Lexer
--import qualified Data.IntMap.Strict as Map
import qualified Data.Set as Set
import qualified Data.Map.Strict as SMap
import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as Map

data MExpr = Anchor Int 
           | MLambda String MExpr
           | MAppl MExpr MExpr
           | MVar String
           deriving (Eq, Ord)

type MMap = Map.HashMap Int MExpr
type MState = State (MMap, Int)
type SMap = Map.HashMap String String
type MSet = Set.Set String

rehydrate :: G.Expr -> MExpr
rehydrate e = smartRehydrate e

smartRehydrate :: G.Expr -> MExpr
smartRehydrate e = case e of
  (Lambda s e) -> 
    MLambda s (smartRehydrate e) 
  (Appl a b) -> 
    MAppl (smartRehydrate a) (smartRehydrate b)
  (Var s) -> MVar s


instance Show MExpr where
  show (MVar x)        = x
  show (MLambda e1 e2) = "(\\" ++ e1 ++ "." ++ show e2 ++ ")"
  show (MAppl e1 e2)   = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Anchor s)    = "@" ++ show s ++ ""


createAnchor :: MExpr -> MState MExpr
createAnchor e = case e of
  (Anchor _) -> return e
  _ -> do 
    i <- snd <$> get 
    let a = Anchor i
    modify (\(mmap, i) -> (Map.insert i e mmap, i + 1))
    return a

getM :: String -> MExpr
getM s = rehydrate $ fromRight undefined $ parseLambda (alexScanTokens s)


doSubstitution :: MExpr -> String -> MSet -> MExpr -> MState MExpr
doSubstitution left var contextVars target = do
  vars <- freeVars target
  let nvars = Set.delete var vars
  doSubstitutionImpl left var target (Set.union vars contextVars) Set.empty

removeCollisions :: MSet -> MSet -> MExpr -> MState (MExpr, Bool)
removeCollisions set met e = case e of
  m@(Anchor i) -> do
    expr <- extractMExpr m
    (newExpr, res) <- removeCollisions set met expr 
    modify (\(m, j) -> (Map.insert i newExpr m, j))
    return (m, res)
  (MAppl e1 e2) -> do
    (left, res1) <- removeCollisions set met e1 
    (right, res2) <- removeCollisions set met e2 
    return $ (MAppl left right, res1 || res2) 
  (MVar v) -> let nv = findEncoded set met v in
    return $ (MVar nv, not (v == nv)) 
  (MLambda s e1) -> do
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
  inside <- unwrapTillNearest left >>= extractMExpr
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
  (MAppl e1 e2) -> do
    left <- freeVars e1
    right <- freeVars e2
    return $ Set.union left right
  (MLambda s e1) -> freeVars e1 >>= return . Set.delete s
  (Anchor i)  -> do 
    expr <- extractMExpr e
    freeVars expr
  (MVar v) -> return (Set.singleton v)


extractMExpr :: MExpr -> MState MExpr
extractMExpr m = case m of
  (Anchor i) -> do
    map <- fst <$> get
    return $ map Map.! i
  m -> return m



findReduction :: MExpr -> MSet -> MState (MExpr, Bool)
findReduction e cvars = do
  case e of
    (MAppl _ _)   -> reduceAppl e cvars
    (MLambda s e) -> (return e) >>= (\e -> tryReduce e (Set.insert s cvars) (\x -> MLambda s x))
    m@(MVar x)    -> return (m, False)
    m@(Anchor s)  -> do 
                     e <- unwrapTillNearest m >>= extractMExpr 
                     (r, b) <- findReduction e cvars
                     if b then do
                               modify (\(map, i) -> (Map.insert s r map, i)) 
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
      expr <- extractMExpr e
      case expr of
        (Anchor _) -> unwrapTillNearest expr
        _ -> return e 
    m -> return m

forceReduceAppl :: MExpr -> MSet -> MState (MExpr, Bool)
forceReduceAppl gm@(MAppl e1 e2) cvars = do 
  ne1 <- extractMExpr e1
  case ne1 of
    (MLambda s innerE) -> do
      newAnchor <- createAnchor e2
      expr <- doSubstitution innerE s cvars newAnchor
      return (expr, True)
    other -> do  
      m1 <- tryReduce e1 cvars (\x -> MAppl x e2)
      if (snd m1) then return m1 
                  else tryReduce e2 cvars (\x -> MAppl other x) 


tryReduce :: MExpr -> MSet -> (MExpr -> MExpr) -> MState (MExpr, Bool)
tryReduce x cvars f = do
  (r, b) <- findReduction x cvars
  return $ if b then (f r, True) else (f x, False)


reduceExpr :: Int -> Int -> Expr -> [Expr]
reduceExpr i k e = let initial = (Map.empty, 0) in
  evalState (iteratedReduction k i (rehydrate e)) initial 

reduceExprDebug :: Int -> Int -> Expr -> [String]
reduceExprDebug i k e = let initial = (Map.empty, 0) in
  evalState (iteratedReductionDebug k i (rehydrate e)) initial


iteratedReduction :: Int -> Int -> MExpr -> MState [Expr]
iteratedReduction step iters initial = iteratedReductionHelper step iters 0 initial 

iteratedReductionDebug :: Int -> Int -> MExpr -> MState [String]
iteratedReductionDebug step iters initial = iteratedReductionHelperDebug step iters 0 initial

iteratedReductionHelper :: Int -> Int -> Int -> MExpr -> MState [Expr]
iteratedReductionHelper step iters counter initial = 
  if iters == 0
  then (dehydrate initial) >>= return . (:[])
  else do
    currentStatement <- dehydrate initial
    (e, b)           <- findReduction initial Set.empty
    let leftIters = if b then iters - 1 else 0 
    list             <- iteratedReductionHelper step leftIters (counter + 1) e
    if counter `mod` step == 0 && b then return $ currentStatement : list
                               else return list


iteratedReductionHelperDebug :: Int -> Int -> Int -> MExpr -> MState [String]
iteratedReductionHelperDebug step iters counter initial = 
  if iters == 0
  then do
    expr <- dehydrate initial
    map <- show <$> fst <$> get
    return [(show initial  ++ " | " ++ map)]
  else do
    map <- show <$> fst <$> get
    currentStatement <- dehydrate initial
    (e, b)           <- findReduction initial Set.empty
    let leftIters = if b then iters - 1 else 0 
    list             <- iteratedReductionHelperDebug step leftIters (counter + 1) e
    if counter `mod` step == 0 && b then return $ (show initial ++" | "  ++ show map) : list
                               else return list


dehydrate :: MExpr -> MState G.Expr
dehydrate e =
  case e of
    (MAppl e1 e2) -> do 
      r1 <- dehydrate e1
      r2 <- dehydrate e2
      return $ Appl r1 r2
    (MLambda s e) -> do
      r <- dehydrate e
      return $ Lambda s r
    (MVar s)      -> return $ Var s
    m@(Anchor i)  -> do
      r <- extractMExpr m
      dehydrate r


--removeEncoding :: G.Expr -> G.Expr
--removeEncoding = fst . smartRemoveEncoding Set.empty

--smartRemoveEncoding :: MSet -> G.Expr -> (G.Expr, MSet)
--smartRemoveEncoding set e = case e of
--  (Lambda s e2) -> let innerSet = smartRemoveEncoding set e2 in
--    if s `Set.member` innerSet then s = tail . tail . s

emptyState :: (MMap, Int)
emptyState = (Map.empty, 0)

fromRight :: a -> Either b a -> a
fromRight x (Left _)  = x
fromRight _ (Right x) = x 

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe x Nothing  = x
inter = "(\\x.x x x) (\\y.(\\z.z) a y)"
sample = "(\\x.x x x x) ((\\y.y) (\\z.z))"
tricky = "(\\x. (\\v.x) x x) ((\\z.z) a)"
tr2 = "(\\d.d z d d) (\\x.(\\r.x) x)"
tr3 = "(\\v.(\\p.p (\\p.v)) v) ((\\a.z) u)"
tr4 = "(\\v.(\\p.p (z p) (\\p.v)) v) (\\n.\\i.n n ((\\a.z) n))"
tr5 = "(\\v.(\\p.p (z p) (\\p.v)) v) (\\n.\\i.n i ((\\a.z) u))"
tr6 = "(\\v.(\\p.v p (\\p.v)) v) (\\n.\\i.n i)"
tr7 = "(\\a.a a) (\\n.w (\\n.(\\t.h) p) n)"
tr8 = "(\\a.a a) (\\n.n (\\v.y))"
tr9 = "\\a.(\\a.\\f.a (\\s.a)) (\\n.\\y.\\v.y (\\v.\\u.u w (\\v.\\n.(\\t.h) (\\y.x)) n))"
tr10 = "(\\a.a (\\s.a)) (\\n.(n v)  (\\n.(\\t.h) p) n)"
tr11 = "(\\u.u (\\g.u g u)) (\\c.\\n.c n)"
tr12 = "(\\b.b (v b) b) (\\z.(\\v.z) i z)"
tr13 = "(\\b.b (b y)) (\\h.(\\z.z (\\z.h)) h)"
tr14 = "(\\e.(\\f.f (\\f.e)) e) ((\\s.u) f)"
tr15 = "(\\o.(\\d.d (\\d.o)) o) ((\\x.x) t)"
tr16 = "((\\w.((\\f.w) w)) ((\\f.f) ((\\k.o) f)))"
tr17 = "(\\u.u u) ((\\w.o) n)"
tr18 = "(\\e.(\\b.e ((\\i.i) e)) t) (b \\b.(\\g.l) t)"
tr19 = "(((\\z.(\\i.((\\x.(x z)) (z (\\o.l))))) (\\l.(\\i.(l l)))) s)"
tr20 = "(\\p.p p) (\\u.\\m.(u (\\u.m)) u)"
tr21 = "(\\e.(\\b.e ((\\i.i) e)) t) (b \\b.(\\g.l) t)"
tr22 = "((\\q.((\\f.(q q)) q)) (((\\y.y)) y))"
tr23 = "((\\a.(a a)) (\\n.((u (\\n.((\\t.h) x))) n)))"
tr24 = "((\\w.((\\u.(u ((\\g.d) a))) ((\\r.(\\l.(\\w.l))) q))) l)"
tr25 = "((\\j.(((\\t.(s j)) x) j)) (\\t.((\\a.t) x)))"
tr26 = "((\\i.((a ((\\h.h) ((\\b.((\\i.b) b)) i))) i)) (\\i.((\\g.i) i)))"
tr27 = "((\\i.((a (((\\b.((\\i.b) b)) i))) i)) (\\i.((\\g.i) i)))"
-- run :: (MExpr -> a) -> String -> a
-- run f = f . getM


-- shw :: Show a => MState a -> String
-- shw x = show $ runState x emptyState

-- unwrp :: MState (MExpr, Bool) -> MState MExpr
-- unwrp e = e >>= (return . fst)

-- mexpr :: MExpr
-- mexpr = getM "(\\x.x x x x) ((\\x.x) (\\x.x))"

-- mt2 = iteratedReduction 2 mexpr

--execProgram :: String -> Int -> Expr
--execProgram s i = executeReductions i (getM s)
