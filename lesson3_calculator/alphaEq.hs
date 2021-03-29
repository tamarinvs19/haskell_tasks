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
    z = freshVar x

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var x) (Var y) = x == y
alphaEq (f :@ x) (g :@ y) = alphaEq f g && alphaEq x y
alphaEq (Lam v t1) (Lam u t2) | v == u = alphaEq t1 t2
                              | v `notElem` freeVars t2 = alphaEq t1 (substVar u v t2)
                              | u `notElem` freeVars t1 = alphaEq (substVar v u t1) t2
                              | otherwise = alphaEq t1 (substVar u v t2)
alphaEq _ _ = False


substVar :: Symb -> Symb -> Expr -> Expr
substVar s s' e = subst s (Var s') e

freeVars :: Expr -> [Symb]
freeVars (Var v) = [v]
freeVars (a :@ b) = freeVars a `union` freeVars b
freeVars (Lam v e) = freeVars e \\ [v]

freshVar :: String -> Symb
freshVar s = "_" ++ s ++ "'"
