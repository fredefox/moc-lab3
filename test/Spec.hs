import Chi
import Interpreter
import GHC.Natural
import Data.Maybe
import Test.QuickCheck

import Debug.Trace

main :: IO ()
main = mapM_ quickCheck [prop_add, prop_mult]

-- `foo` implements equality tests on natural numbers.
sample = parse . unlines $
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
