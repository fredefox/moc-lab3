# Question 1
The given program implements equality for natural numbers (defined in the usual way).

It would be possible to give a formal proof of this property, I have however elected to
write a QuickCheck-test for it. Please see `foo` and `prop_foo` in `Spec.hs`.

# Question 2
Please see `Interpreter.subst`.

The implementation is tested with the function given in `Chi.hs` thus: `testSubstitution subst`.
See `Spec.hs` for more info.

# Question 3
Please see `mult` and `prop_mult` in `Spec.hs`.

Please note that the the interpreter is hopelessly ineffecient. multiplication appears to
have an exponential runtime.[^1] This also means that running `main` as defined in `Spec.hs`
will likely result in your CPU getting hot for a long time.

[^1]: One thought I had to alleviate I had to alleviate this was to perhaps only reduce
      terms to head normal form when only that is needed. I have not given this more thought
      though.

# Question 4
An interpreter for Chi has been implemented as `Interpreter.interpret`.

The implementation is tested with the function given in `Chi.hs` thus: `testEvaluation intepret`.
See `Spec.hs` for more info.

Addition (as well as multiplication) has been tested with this implementation using QuickCheck
tests. Please see `prop_add` in `Spec.hs`.
