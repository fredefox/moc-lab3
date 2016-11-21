module Interpreter
    ( interpret
    ) where

import Chi

subst :: Variable -> Exp -> Exp -> Exp
subst v e t = case t of
  Apply e0 e1  -> Apply (subst v e e0) (subst v e e1)
  Lambda v0 e0 -> if v == v0 then t else (Lambda v0 (subst v e e0))
  Case e0 bs   -> Case (subst v e e0) (map substB bs)
    where
      substB b@(Branch c vs e1) =
        if any (== v) vs
        then b
        else Branch c vs (subst v e e1)
  Rec v0 e0    -> if v == v0 then t else (Rec v0 (subst v e e0))
  Var v0       -> if v == v0 then e else t
  Const _ _    -> t

interpret :: IO ()
interpret = error "Unimplemented"
