module SelfInterpreter
    ( subst
    , interpret
    ) where

import Chi
import qualified Interpreter as I
import Data.List
import Data.Maybe

subst     :: Exp
subst     = withGlobalDefs $ Var (Variable "subst")

interpret :: Exp
interpret = withGlobalDefs $ Var (Variable "interpret")

-----------------------
-- Auxillary methods --
-----------------------
withScope :: Exp -> [(Variable, Exp)] -> Exp
withScope = foldl step
  where
    step t (var, e) = Apply (Lambda var t) e

globalDefs :: [(Variable, Exp)]
globalDefs =
  [ (Variable "subst",     substChi)
  , (Variable "equal",     equalChi)
  , (Variable "bool",      boolChi)
  , (Variable "elem",      elemChi)
  , (Variable "map",       mapChi)
  --, (Variable "interpret", interpretChi)
  ]

withGlobalDefs :: Exp -> Exp
withGlobalDefs e = e `withScope` globalDefs

---------------------
-- Chi-expressions --
---------------------
-- A small helper function
parseA :: [String] -> Exp
parseA = parse . unlines

substChi :: Exp
substChi = parseA
  [ "rec subst = \\v . \\e . \\t . case t of"
  , "  { Apply(e0, e1)  -> Apply(subst v e e0, subst v e e1)"
  , "  ; Lambda(v0, e0) -> bool Lambda(v0, subst v e e0) t (equal v v0)"
  , "  ; Case(e0, bs)   -> Case(subst v e e0, map (" ++ substB ++ ") bs)"
  , "  ; Rec(v0, e0)    -> bool Rec(v0, subst v e e0) t (equal v v0)"
  , "  ; Var(v0)        -> bool t e (equal v v0)"
  , "  ; Const(c, es)   -> Const(c, map (\\x . subst v e x) es)"
  , "  }"
  ]
  where
    substB = unlines
      [ "\\b . case b of"
      , "  { Branch(c, vs, e1) -> bool b Branch(c, vs, subst v e e1) (elem v vs)"
      , "  }"
      ]

equalChi :: Exp
equalChi = parseA
  [ "rec equal = \\a . \\b . case a of"
  , "  { Zero()   -> case b of"
  , "    { Zero()   -> True()"
  , "    ; Succ(b') -> False()"
  , "    }"
  , "  ; Succ(a') -> case b of"
  , "    { Zero()   -> False()"
  , "    ; Succ(b') -> equal a' b'"
  , "    }"
  , "  }"
  ]

boolChi :: Exp
boolChi = parseA
  [ "\\f . \\t . \\cond . case cond of"
  , "  { False() -> f"
  , "  ; True()  -> t"
  , "  }"
  ]

elemChi :: Exp
elemChi = parseA
  [ "rec elem = \\a . \\t . case xs of"
  , "  { Nil() -> False()"
  , "  ; Cons(x, xs) -> bool (elem a xs) True() (equal a x)"
  , "  }"
  ]

mapChi :: Exp
mapChi = parseA
  [ "rec map = \\f . \\xs . case xs of"
  , "  { Nil()       -> Nil()"
  , "  ; Cons(x, xs) -> Cons(f x, map f xs)"
  , "  }"
  ]

interpretChi :: Exp
interpretChi = parseA
  $ error "Unimplemented"
