import Chi
import Interpreter
import qualified SelfInterpreter
import GHC.Natural
import Data.Maybe
import Test.QuickCheck

import Debug.Trace

main :: IO ()
main = quickCheck prop_foo
    >> testSubstitution subst
    >> testEvaluation interpret
    >> quickCheck prop_add
    >> quickCheck prop_mult

-- `foo` implements equality tests on natural numbers.
foo :: Exp
foo = parse . unlines $
  [ "rec foo = \\m. \\n. case m of"
  , "  { Zero() -> case n of"
  , "    { Zero()  -> True()"
  , "    ; Succ(n) -> False()"
  , "    }"
  , "  ; Succ(m) -> case n of"
  , "    { Zero()  -> False()"
  , "    ; Succ(n) -> foo m n"
  , "    }"
  , "  }"
  ]

prop_foo :: Natural -> Natural -> Bool
prop_foo n m = fooHelper n m == Just (n == m)

toBool :: Exp -> Maybe Bool
toBool (Const c _) = Just $ case c of
  Constructor "False" -> False
  Constructor "True"    -> True
toBool _ = Nothing

fooHelper :: Natural -> Natural -> Maybe Bool
fooHelper n m = toBool $ applyN foo nm
  where
    nm = map fromNatural [n, m]

mult :: Exp
mult = subst (Variable "add") add multimpl
  where
    multimpl = parse . unlines $
      [ "rec mult = \\ m. \\ n. case m of"
      , "  { Zero()  -> Zero()"
      , "  ; Succ(m) -> add n (mult m n)"
      , "  }"
      ]

applyN :: Exp -> [Exp] -> Exp
applyN e es = interpret $ foldl Apply e es

applyNum :: Exp -> [Natural] -> Maybe Natural
applyNum e = toNatural . applyN e . map fromNatural

addHelper :: Natural -> Natural -> Maybe Natural
addHelper n m = applyNum add [n, m]

multHelper :: Natural -> Natural -> Maybe Natural
multHelper n m = applyNum mult [n, m]

prop_add :: Natural -> Natural -> Bool
prop_add n m = addHelper n m == Just (n + m)

prop_mult :: Natural -> Natural -> Bool
prop_mult n m = multHelper n m == Just (n * m)

testSelfSubstitution :: Variable -> Exp -> Exp -> Bool
testSelfSubstitution v e0 e1
  =  interpret (subst v e0 e1)
  == interpret self
  where
    self = Apply (Apply (Apply SelfInterpreter.subst (Var v)) e0) e1

hlpr v e0 e1 = Apply (Apply (Apply SelfInterpreter.subst (Var v)) e0) e1
