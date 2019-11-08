module Reduction where
import Grammar as G
import Parser
import Lexer
import qualified Data.IntMap.Strict as Map
import qualified Data.Set as Set
import qualified Data.Map.Strict as SMap
import Control.Monad.State.Lazy

data MExpr = Anchor Int
           | MLambda String MExpr
           | MAppl MExpr MExpr
           | MVar String
           deriving (Eq, Ord)

type MMap = Map.IntMap MExpr
type MState = State (MMap, Int)
type SMap = SMap.Map String String

rehydrate :: G.Expr -> MExpr
rehydrate e = smartRehydrate e SMap.empty

encode :: String -> String
encode = (++ "1337''''")

smartRehydrate :: G.Expr -> SMap -> State MExpr
smartRehydrate (G.Lambda s e) set = let newSet = SMap.insert s (encode set) set in
  MLambda (encode s) (smartRehydrate e newSet)
smartRehydrate (G.Appl e i) set = MAppl (smartRehydrate e set) (smartRehydrate i set)
smartRehydrate (G.Var s) set = if s `Set.member` set then MVar (encode s) else MVar s


instance Show MExpr where
  show (MVar x)        = x
  show (MLambda e1 e2) = "(\\" ++ e1 ++ "." ++ show e2 ++ ")"
  show (MAppl e1 e2)   = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Anchor s)      = "(@" ++ show s ++ ":)"


createAnchor :: MExpr -> MState MExpr
createAnchor e = do
  i <- snd <$> get 
  let a = Anchor i
  modify (\(mmap, i) -> (Map.insert i e mmap, i + 1))
  return a

getM :: String -> MExpr
getM s = rehydrate $ fromRight undefined $ parseLambda (alexScanTokens s)


doSubstitution :: MExpr -> String -> MExpr -> MState MExpr
doSubstitution left var target = do
  let runSubst x = doSubstitution x var target
  case left of
    (Anchor i) -> do 
                  mmap <- fst <$> get
                  runSubst (mmap Map.! i)
    l@(MLambda s e) -> if var == s 
                       then return l 
                       else do
                         expr <- runSubst e
                         return $ MLambda s expr
    (MAppl e1 e2) -> do
                     expr1 <- runSubst e1
                     expr2 <- runSubst e2
                     return $ MAppl expr1 expr2
    v@(MVar s) -> return $ if var == s then target else v


unwrapNearestLambda :: MExpr -> MState MExpr
unwrapNearestLambda e = do
  case e of
    (Anchor i) -> do
      mmap <- fst <$> get
      let nested = mmap Map.! i
      case nested of
        m@(Anchor j)    -> unwrapNearestLambda m
        m@(MLambda _ _) -> return m
        m               -> return (Anchor i) 
    s          -> return s

findReduction :: MExpr -> MState (MExpr, Bool)
findReduction e = do
  case e of
    (MAppl e1 e2) -> do
      ne1 <- unwrapNearestLambda e1
      case ne1 of
        (MLambda s e1) -> do
          newAnchor <- createAnchor e2
          expr      <- doSubstitution e1 s newAnchor
          return (expr, True)
        e1 -> do  
          m1 <- tryReduce e1 (\x -> MAppl x e2)
          if (snd m1) then return m1 
                      else tryReduce e2 (\x -> MAppl e1 x) 
    (MLambda s e) -> tryReduce e (\x -> MLambda s x)
    m@(MVar x)    -> return (m, False)
    m@(Anchor s)  -> do 
                     mmap <- fst <$> get
                     let e = mmap Map.! s
                     (r, b) <- findReduction e
                     if b then do
                               modify (\(m, i) -> (Map.insert s r m, i)) 
                               return (Anchor s, True)
                     else do 
                            return (m, False) 

tryReduce :: MExpr -> (MExpr -> MExpr) -> MState (MExpr, Bool)
tryReduce x f = do
  (r, b) <- findReduction x
  return $ if b then (f r, True) else (f x, False)


reduceExpr :: Int -> Int -> Expr -> [Expr]
reduceExpr i k e = let initial = (Map.empty, 0) in
  evalState (iteratedReduction k i (rehydrate e)) initial 


iteratedReduction :: Int -> Int -> MExpr -> MState [Expr]
iteratedReduction step iters initial = iteratedReductionHelper step iters 0 initial 

iteratedReductionHelper :: Int -> Int -> Int -> MExpr -> MState [Expr]
iteratedReductionHelper step iters counter initial = 
  if iters == 0
  then (dehydrate initial) >>= return . (:[])
  else do
    currentStatement <- dehydrate initial
    (e, b)           <- findReduction initial
    let leftIters = if b then iters - 1 else 0 
    list             <- iteratedReductionHelper step leftIters (counter + 1) e
    if counter `mod` step == 0 && b then return $ currentStatement : list
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
    (Anchor i)    -> do
      mmap <- fst <$> get
      let r = mmap Map.! i
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
