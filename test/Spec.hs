import Chi

main :: IO ()
main = putStrLn "Test suite not yet implemented"

-- `foo` implements equality tests on natural numbers.
sample = parse . unlines $
  [ "rec foo = λm. λn. case m of"
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
