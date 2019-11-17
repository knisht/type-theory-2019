module Unification where
import Types
import Data.IntMap as Map
import Control.Applicative

type SMap = IntMap SType
type REquation = (Equation, Bool {-is unique-})

unify :: [Equation] -> Maybe SMap
unify list = toMap <$> (phasedIteration $ rehydrate list)
 
phasedIteration :: [REquation] -> Maybe [REquation]
phasedIteration list = let phased = unificationPhase list in
 case phased of
    Nothing -> Just list
    Just a  -> phasedIteration a 

toMap :: [REquation] -> SMap
toMap [] = Map.empty
toMap (((Atom i) := b, _) : tail) = Map.insert i b $ toMap tail 


rehydrate :: [Equation] -> [REquation]
rehydrate = ((\x -> (x,False)) <$>)


-- todo: contradiction
unificationPhase :: [REquation] -> Maybe [REquation]
unificationPhase list = let (left, right, target) = unificationPhasePassing [] list in
  (\e -> substituteDual left right e) <$> target

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
