import Chi
import Interpreter

main :: IO ()
main = putStrLn "Test suite not yet implemented"

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
      , "  { Zero()  -> n"
      , "  ; Succ(m) -> add n (mult m n)"
      , "  }"
      ]
