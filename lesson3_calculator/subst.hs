import Data.List
type Symb = String
infixl 2 :@

data Expr = Var Symb | Expr :@ Expr | Lam Symb Expr
          deriving (Eq, Read, Show)

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
  then freshVar ls (s ++ "'")
  else freshVar ls s

subst :: Symb -> Expr -> Expr -> Expr
subst x n t@(Var symb) | x == symb = n
                       | otherwise = t 
subst x n (p :@ q) = (subst x n p) :@ (subst x n q)
subst x n @t(Lam y p) 
  | y == x = t
  | y `notElem` (freeVars n) = Lam y (subst x n p)
  | y `elem` (freeVars n) = subst x n (Lam z (subst y (Var z) p))
      where z = freshVar ((allVars n) `union` (allVars p)) "_x"

res = subst "y"  (Var "x")  (Lam "x" $ (Var "x") :@ (Var "y") :@ (Lam "x" $ (Var "x") :@ (Var "y")))
