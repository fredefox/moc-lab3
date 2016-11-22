module Interpreter
    ( interpret
    , subst
    ) where

import Chi
import Data.List
import Data.Maybe

subst :: Variable -> Exp -> Exp -> Exp
subst v e t = case t of
  Apply e0 e1  -> Apply (subst v e e0) (subst v e e1)
  Lambda v0 e0 -> if v == v0 then t else Lambda v0 (subst v e e0)
  Case e0 bs   -> Case (subst v e e0) (map substB bs)
    where
      substB b@(Branch c vs e1) =
        if v `elem` vs
        then b
        else Branch c vs (subst v e e1)
  Rec v0 e0    -> if v == v0 then t else Rec v0 (subst v e e0)
  Var v0       -> if v == v0 then e else t
  Const c es   -> Const c $ map (subst v e) es

interpret :: Exp -> Exp
interpret e = case e of
  Apply e0 e1  -> interpret $ subst x e1' e
    where
      (Lambda x e) = interpret e0
      e1' = interpret e1
  Lambda _ _   -> e
  Case e bs    -> interpret e''
    where
      (Const c es) = interpret e
      (Branch _ xs e') = findB c bs
      e''          = substMany xs es e'
  Rec v e'     -> interpret $ subst v e e'
  Var _        -> e
  Const c es   -> Const c $ map interpret es

findB :: Constructor -> [Br] -> Br
findB c = fromMaybe undefined . find (\(Branch c' _ _) -> c == c')

substMany :: [Variable] -> [Exp] -> Exp -> Exp
substMany vs es e0 = foldl step e0 $ zip vs es
  where
    step acc (v, e) = subst v e acc
