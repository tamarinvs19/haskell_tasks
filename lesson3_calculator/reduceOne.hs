import Data.List

type Symb = String
infixl 2 :@
infix 1 `alphaEq`

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
          deriving (Eq, Read, Show)

subst :: Symb -> Expr -> Expr -> Expr
subst v n m@(Var x) | v == x = n
  | otherwise = m
subst v n (a :@ b) = (subst v n a) :@ (subst v n b)
subst v n m@(Lam x term) | x == v = m
  | x `notElem` freeVars n = Lam x $ subst v n term
  | otherwise = subst v n $ Lam z (subst x (Var z) term)
  where
    z = freshVar (allVars n `union` allVars m) "new"

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var x) (Var y) = x == y
alphaEq (a :@ b) (c :@ d) = alphaEq a c && alphaEq b d
alphaEq (Lam x n) (Lam y m) = alphaEq n (subst y (Var x) m)
alphaEq _ _ = False

reduceOnce :: Expr -> Maybe Expr
reduceOnce (Var a) = Nothing
reduceOnce (Lam x term) = reduceOnce term >>= \t -> return (Lam x t)
reduceOnce ((Lam x term) :@ val) = Just $ subst x val term
reduceOnce (a :@ b) = case reduceOnce a of
        Just a -> Just $ a :@ b
        Nothing -> reduceOnce b >>= \t -> return $ a :@ t


freeVars :: Expr -> [Symb]
freeVars (Var v) = [v]
freeVars (a :@ b) = freeVars a `union` freeVars b
freeVars (Lam v e) = freeVars e \\ [v]

boundVars :: Expr -> [Symb]
boundVars (Var v) = []
boundVars (a :@ b) = freeVars a `union` freeVars b
boundVars (Lam v e) = (freeVars e) `union` [v]

allVars :: Expr -> [Symb]
allVars e = freeVars e `union` boundVars e

freshVar :: [Symb] -> String -> Symb
freshVar [] s = s
freshVar (l:ls) s =
  if l == s
  then freshVar ls (s ++ "_" ++ l)
  else freshVar ls s

