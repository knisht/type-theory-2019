module Unification where
import Types
import Data.IntMap as Map
import Control.Applicative
import Debug

type SMap = IntMap SType
type REquation = (Equation, Bool {-is unique-})

data Triple a = TNoth | TErr | TJust a

instance Functor Triple where
  fmap f t = case t of
    TJust a -> TJust $ f a
    TNoth -> TNoth
    TErr -> TErr

unify :: [Equation] -> Maybe SMap
unify list = toMap <$> ((phasedIteration $ rehydrate list))
 

checkSanity :: [REquation] -> Maybe [REquation]
checkSanity [] = return []
checkSanity (e : es) = liftA2 (:) (ensureSanity e) (checkSanity es)

ensureSanity :: REquation -> Maybe REquation
ensureSanity m@(((Atom i) := b), _) = if findIndex i b then Nothing else return m
ensureSanity m = return m

findIndex :: Int -> SType -> Bool
findIndex i (Atom j) = j == i
findIndex i (a :-> b) = findIndex i a || findIndex i b

phasedIteration :: [REquation] -> Maybe [REquation]
phasedIteration list = let phased = unificationPhase list in
 case phased of
    TNoth -> Just list
    TErr -> Nothing
    TJust a  -> phasedIteration a 

toMap :: [REquation] -> SMap
toMap [] = Map.empty
toMap (((Atom i) := b, _) : tail) = Map.insert i b $ toMap tail 
toMap (e : tail) = toMap tail

rehydrate :: [Equation] -> [REquation]
rehydrate = ((\x -> (x,False)) <$>)

fromMaybeToTriple :: Maybe REquation -> Triple REquation
fromMaybeToTriple m = case m of
  Just a -> case ensureSanity a of
    Just a -> TJust a
    Nothing -> TErr
  Nothing -> TNoth

unificationPhase :: [REquation] -> Triple [REquation]
unificationPhase list = let (left, right, target) = unificationPhasePassing [] list in
  (\e -> substituteDual left right e) <$> (fromMaybeToTriple target)

unificationPhasePassing :: [REquation] -> [REquation] -> ([REquation], [REquation], Maybe REquation)
unificationPhasePassing l [] = (l, [], Nothing)
unificationPhasePassing l (e : tail) = case simpleCheck e of 
  (Just r) -> case r of 
    [(_, True)] -> unificationPhasePassing (r ++ l) tail
    _           -> unificationPhasePassing l (r ++ tail)
  Nothing  -> case e of
    (Atom a := b, False) -> (l, tail, pure e)
    _ -> unificationPhasePassing (e : l) tail

type RAlt = REquation -> Maybe [REquation]

simpleCheck :: RAlt
simpleCheck r = pass r <|> duplicate r <|> swap r <|> unwrap r 

pass :: RAlt
pass e = case e of 
  (_, True) -> return [e]
  _         -> mempty 

duplicate :: RAlt
duplicate (Atom v := Atom u, _) = if v == u then return [] else Nothing
duplicate _ = Nothing  
   
swap :: RAlt
swap r@(e := Atom u, b) = case e of 
  Atom _ -> mempty
  _ :-> _ -> return $ return $ (Atom u := e, b)
swap _ = mempty

unwrap :: RAlt
unwrap ((a :-> b) := (c :-> d), ind) = return $ [(a := c, ind), (b := d, ind)]
unwrap _ = Nothing


substituteDual :: [REquation] -> [REquation] -> REquation -> [REquation]
substituteDual l r ((Atom a) := b, _) = let partialSubstitute targ accum = substitute a b targ accum in
  ((Atom a := b), True) : (partialSubstitute l $ partialSubstitute r [])


substitute :: Int -> SType -> [REquation] -> [REquation] -> [REquation]
substitute _ _ [] l = l
substitute i repl (a : tail) l = substitute i repl tail (immediateSubstitute i repl a : l)

immediateSubstitute :: Int -> SType -> REquation -> REquation
immediateSubstitute i repl (a := b, ind) = (innerSubstitute i repl a := innerSubstitute i repl b, ind)

innerSubstitute :: Int -> SType -> SType -> SType
innerSubstitute i repl e = case e of
  Atom j  -> if j == i then repl else e
  a :-> b -> innerSubstitute i repl a :-> innerSubstitute i repl b  
