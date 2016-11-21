-- | A wrapper module that exports the generated abstract syntax of χ
-- as well as some definitions that may be useful when testing χ code
-- or interpreters.

{-# LANGUAGE DeriveGeneric,
             GeneralizedNewtypeDeriving,
             StandaloneDeriving,
             TemplateHaskell #-}

module Chi
  ( -- * The abstract syntax of χ
    module AbsChi
    -- * Parsing and pretty-printing
  , parse
  , pretty
    -- * An example
  , add
    -- * Conversions
  , fromNatural
  , toNatural
  , Code
  , Decode
  , runCode
  , runDecode
  , asDecoder
  , code
  , decode
  , codeVar
  , codeCon
  , decodeVar
  , decodeCon
    -- * Free and bound variables
  , free
  , bound
  , closed
    -- * Self-interpreters
  , runSelfInterpreter
    -- * Some testing procedures
  , testEvaluation
  , testMultiplication
  , testSubstitution
  , testInternalSubstitution
  , testSelfInterpreterWith
  , testSelfInterpreter
  )
  where

import AbsChi
import Control.Applicative
  (Applicative, Alternative, (<$>), (<*>), empty)
import Control.Monad.State.Strict (State, evalState, put, get)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.Typeable (Typeable)
import ErrM (Err(..))
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import ParChi (pExp, myLexer)
import PrintChi (printTree)
import Test.QuickCheck
  (Arbitrary(arbitrary, shrink), Gen, NonNegative(NonNegative),
   (==>), (.&&.), choose, elements, forAll, oneof, sized, vectorOf,
   genericShrink, quickCheckAll, quickCheckResult)
import Test.QuickCheck.Test (isSuccess)

------------------------------------------------------------------------
-- Some orphan instances for types defined in Abschi

deriving instance Generic Constructor
deriving instance Generic Variable
deriving instance Generic Exp
deriving instance Generic Br

instance Hashable Constructor
instance Hashable Variable

------------------------------------------------------------------------
-- Orphan Arbitrary instances

-- | A QuickCheck generator for (a small number of) constructors.

instance Arbitrary Constructor where
  arbitrary = elements $ map Constructor ["Zero", "Succ"]

  shrink (Constructor "Succ") = [Constructor "Zero"]
  shrink _                    = []

-- | A QuickCheck generator for (a small number of) variables.

instance Arbitrary Variable where
  arbitrary = elements $ map Variable ["x", "y"]

  shrink (Variable "y") = [Variable "x"]
  shrink _              = []

-- | A QuickCheck generator for branches.

instance Arbitrary Br where
  arbitrary = Branch <$> arbitrary <*> arbitrary <*> arbitrary

  shrink = genericShrink

-- | A QuickCheck generator for arbitrary terms.

instance Arbitrary Exp where
  arbitrary = sized gen
    where
    gen n = oneof $
      [ Var <$> arbitrary ] ++
      if n <= 0 then [] else
        [ Apply <$> g <*> g
        , Lambda <$> arbitrary <*> g
        , Case <$> g <*> do
            i  <- choose (0, n `div` 3)
            es <- vectorOf i g
            mapM (\e -> Branch <$> arbitrary
                               <*> do i <- choose (0, n `div` 3)
                                      vectorOf i arbitrary
                               <*> return e) es
        , Rec <$> arbitrary <*> g
        , Const <$> arbitrary <*> do
            n  <- choose (0, n `div` 3)
            vectorOf n g
        ]
      where
      g = gen (n `div` 2)

  shrink = genericShrink

------------------------------------------------------------------------
-- Parsing and pretty-printing

-- | Tries to parse a χ program. If parsing fails, then an exception
-- is raised.

parse :: String -> Exp
parse s = case pExp (myLexer s) of
  Ok  e -> e
  Bad s -> error ("Parse failed: " ++ s)

-- | Pretty-prints a χ program.

pretty :: Exp -> String
pretty = printTree

prop_parse_pretty e =
  parse (pretty e) == e

------------------------------------------------------------------------
-- An example

-- | A χ program for addition of natural numbers.

add :: Exp
add = parse
  "rec add = \\x. \\y. case x of\
  \  { Zero()  -> y\
  \  ; Succ(n) -> Succ(add n y)\
  \  }"

------------------------------------------------------------------------
-- Free and bound variables

-- | Helper function used to implement 'free' and 'bound'.

freeAndBound :: Exp -> (HashSet Variable, HashSet Variable)
freeAndBound e = case e of
  Var x       -> (Set.singleton x, Set.empty)
  Lambda x e  -> let (f, b) = freeAndBound e in
                 (Set.delete x f, if   Set.member x f
                                  then Set.insert x b
                                  else b)
  Rec x e     -> freeAndBound (Lambda x e)
  Case e bs   -> freeAndBoundL (e : map (\(Branch _ xs e) ->
                                            foldr Lambda e xs)
                                        bs)
  Apply e1 e2 -> freeAndBoundL [e1, e2]
  Const _ es  -> freeAndBoundL es
  where
  freeAndBoundL :: [Exp] -> (HashSet Variable, HashSet Variable)
  freeAndBoundL es = (Set.unions fs, Set.unions bs)
    where (fs, bs) = unzip $ map freeAndBound es

-- | The term's free variables.

free :: Exp -> HashSet Variable
free e = fst (freeAndBound e)

-- | Bound variables that actually occur bound in the term (not in a
-- binder). For @λx.x@ a singleton set containing @x@ is returned, but
-- for @λx.Nil()@ the empty set is returned.

bound :: Exp -> HashSet Variable
bound e = snd (freeAndBound e)

-- | A QuickCheck generator for closed terms.

closed :: Gen Exp
closed = do
  e <- arbitrary
  let xs = free e
  bs <- mapM binder (Set.toList xs)
  return $ foldr ($) e bs
  where
  binder x = elements [Rec x, Lambda x]

prop_closed =
  forAll closed $ \e ->
    free e == Set.empty

------------------------------------------------------------------------
-- Conversions

-- | Converts a Haskell natural number to its χ representation.

fromNatural :: Natural -> Exp
fromNatural n
  | n == 0    = Const (Constructor "Zero") []
  | otherwise = Const (Constructor "Succ") [fromNatural (n - 1)]

-- | Tries to convert the given χ expression to a natural number.

toNatural :: Exp -> Maybe Natural
toNatural e = case e of
  Const (Constructor "Zero") []  -> Just 0
  Const (Constructor "Succ") [n] -> succ <$> toNatural n
  _                              -> Nothing

prop_toNatural_fromNatural n =
  toNatural (fromNatural n) == Just n

-- | Partial bijections between strings and natural numbers.

data PartialBijection =
  Maps (HashMap String Natural)
       (HashMap Natural String)

-- | Encoding monad.

newtype Code a = Code (State PartialBijection a)
  deriving (Functor, Applicative, Monad)

-- | Decoding monad.

newtype Decode a = Decode (MaybeT Code a)
  deriving (Functor, Applicative, Alternative, Monad)

-- | Runs a coder.

runCode :: Code a -> a
runCode (Code m) = evalState m (Maps Map.empty Map.empty)

-- | Runs a decoder. Note that decoders can fail.

runDecode :: Decode a -> Maybe a
runDecode (Decode m) = runCode (runMaybeT m)

-- | Turns a coder into a decoder.
--
-- Example usage:
--
-- @
--   do e <- 'asDecoder' ('code' e)
--      'decode' (eval ('Apply' selfInterpreter e))
-- @

asDecoder :: Code a -> Decode a
asDecoder = Decode . lift

-- | Converts a string to a natural number.

fromString :: String -> Code Natural
fromString s = do
  Maps strMap natMap <- Code get
  case Map.lookup s strMap of
    Just n  -> return n
    Nothing -> do
      let n = fromIntegral $ Map.size strMap
      Code (put (Maps (Map.insert s n strMap)
                      (Map.insert n s natMap)))
      return n

-- | Tries to convert a natural number to a string.

toString :: Natural -> Decode String
toString n = Decode $ MaybeT $ Code $ do
  Maps _ natMap <- get
  return (Map.lookup n natMap)

-- | Encodes strings.

codeString :: String -> Code Exp
codeString s = fromNatural <$> fromString s

-- | Decodes strings.

decodeString :: Exp -> Decode String
decodeString e = do
  n <- Decode (MaybeT (return (toNatural e)))
  toString n

-- | Encodes variables.

codeVar :: Variable -> Code Exp
codeVar (Variable s) = codeString s

-- | Decodes variables.

decodeVar :: Exp -> Decode Variable
decodeVar e = Variable <$> decodeString e

-- | Encodes constructors.

codeCon :: Constructor -> Code Exp
codeCon (Constructor s) = codeString s

-- | Decodes constructors.

decodeCon :: Exp -> Decode Constructor
decodeCon e = Constructor <$> decodeString e

-- | Encodes expressions.

code :: Exp -> Code Exp
code e = case e of
  Apply e1 e2 -> constr "Apply" [code e1, code e2]
  Lambda x e  -> constr "Lambda" [codeVar x, code e]
  Case e bs   -> constr "Case" [code e, codeList codeBr bs]
  Rec x e     -> constr "Rec" [codeVar x, code e]
  Var x       -> constr "Var" [codeVar x]
  Const c es  -> constr "Const" [codeCon c, codeList code es]
  where
  constr c args = Const (Constructor c) <$> sequence args

  codeBr :: Br -> Code Exp
  codeBr (Branch c xs e) =
    constr "Branch" [codeCon c, codeList codeVar xs, code e]

  codeList :: (a -> Code Exp) -> [a] -> Code Exp
  codeList r []       = constr "Nil" []
  codeList r (x : xs) = constr "Cons" [r x, codeList r xs]

-- | Decodes expressions.

decode :: Exp -> Decode Exp
decode e = case e of
  Const (Constructor c) args -> case (c, args) of
    ("Apply", [e1, e2]) -> Apply <$> decode e1 <*> decode e2
    ("Lambda", [x, e])  -> Lambda <$> decodeVar x <*> decode e
    ("Case", [e, bs])   -> Case <$> decode e <*> decodeList decodeBr bs
    ("Rec", [x, e])     -> Rec <$> decodeVar x <*> decode e
    ("Var", [x])        -> Var <$> decodeVar x
    ("Const", [c, es])  -> Const <$> decodeCon c
                                 <*> decodeList decode es
    _                   -> empty
  _ -> empty
  where
  decodeBr :: Exp -> Decode Br
  decodeBr (Const (Constructor "Branch") [c, xs, e]) =
    Branch <$> decodeCon c <*> decodeList decodeVar xs <*> decode e
  decodeBr _ = empty

  decodeList :: (Exp -> Decode a) -> Exp -> Decode [a]
  decodeList decode e = case e of
    Const (Constructor c) args -> case (c, args) of
      ("Nil", [])       -> return []
      ("Cons", [x, xs]) -> (:) <$> decode x <*> decodeList decode xs

prop_decode_code e =
  runDecode (decode =<< asDecoder (code e)) == Just e

------------------------------------------------------------------------
-- Self-interpreters

-- | Uses a self-interpreter to evaluate a program applied to some
-- arguments.
--
-- If the self-interpreter terminates, and the result can be decoded,
-- then this result is returned.
--
-- Example usage:
--
-- @
--   do n <- 'runSelfInterpreter' eval selfInterpreter 'add'
--             ['fromNatural' 2, 'fromNatural' 2]
--      'toNatural' n
-- @

runSelfInterpreter
  :: (Exp -> Exp)  -- ^ An implementation of evaluation.
  -> Exp           -- ^ The self-interpreter.
  -> Exp           -- ^ The program to be evaluated (not in coded form).
  -> [Exp]         -- ^ The program's arguments (not in coded form).
  -> Maybe Exp
runSelfInterpreter eval selfInterpreter p args = runDecode $ do
  term <- asDecoder (code (foldl1 Apply (p : args)))
  decode (eval (Apply selfInterpreter term))

------------------------------------------------------------------------
-- Some testing procedures

-- | Tests whether a purported implementation of evaluation can
-- evaluate addition of two natural numbers ('add') correctly.
--
-- The test is, of course, incomplete.

testEvaluation :: (Exp -> Exp) -> IO Bool
testEvaluation eval =
  isSuccess <$>
    quickCheckResult (\m n ->
      eval (Apply (Apply add (fromNatural m)) (fromNatural n)) ==
      fromNatural (m + n))

-- | Tests whether a purported implementation of evaluation and a
-- purported implementation of multiplication of two natural numbers
-- are correct.
--
-- The test is, of course, incomplete.

testMultiplication
  :: (Exp -> Exp)  -- ^ Evaluation.
  -> Exp           -- ^ Multiplication. The program should take two
                   --   natural numbers as input and produce a natural
                   --   number as its result.
  -> IO Bool
testMultiplication eval mul =
  isSuccess <$>
    quickCheckResult (\m' n' ->
      let m = m' `div` 2; n = n' `div` 2 in
      eval (Apply (Apply mul (fromNatural m)) (fromNatural n)) ==
      fromNatural (m * n))

-- | Tests a purported implementation of substitution.
--
-- The substitution operation takes three arguments, @x@, @e@ and
-- @e'@, where @e@ is closed, and should substitute @e@ for every free
-- occurrence of @x@ in @e'@.
--
-- The test is designed for substitution operations that are
-- implemented exactly like in the χ specification. In particular, the
-- substitution operation should not rename any bound variables.
--
-- The test is, of course, incomplete.

testSubstitution :: (Variable -> Exp -> Exp -> Exp) -> IO Bool
testSubstitution subst =
  isSuccess <$>
    quickCheckResult
      (\e' ->
        forAll closed $ \e ->
        let fs     = free e'
            fbs    = fs `Set.intersection` bound e'
            var ys = elements (Set.toList ys)
        in
        (forAll (var (Set.insert (Variable "x") fs)) $ \x ->
           free (subst x e e') == Set.delete x fs)
          .&&.
        (not (Set.null fs) && not (isVar e) ==>
         forAll (var fs) $ \x ->
           subst x e e' /= e')
          .&&.
        (not (Set.null fbs) ==>
         forAll (var fbs) $ \x ->
           bound (subst x e e')
             ==
           bound e `Set.union` bound e'))
  where
  isVar Var{} = True
  isVar _     = False

-- | Tests whether an implementation of internal substitution matches
-- an implementation of substitution.
--
-- The test is, of course, incomplete.

testInternalSubstitution
  :: (Exp -> Exp)                     -- ^ Evaluation.
  -> (Variable -> Exp -> Exp -> Exp)  -- ^ Substitution.
  -> Exp                              -- ^ Internal substitution.
  -> IO Bool
testInternalSubstitution eval subst internalSubst =
  isSuccess <$>
    quickCheckResult (\e' ->
      forAll closed $ \e ->
      forAll (elements $ Set.toList $
                Set.insert (Variable "x") $ free e') $ \x ->
        Just (subst x e e')
          ==
        runDecode (do
          x       <- asDecoder $ codeVar x
          [e, e'] <- mapM (asDecoder . code) [e, e']
          decode (eval (foldl1 Apply [internalSubst, x, e, e']))))

-- | Tests if the result of the given self-interpreter matches that of
-- the given evaluator for a program applied to some arguments.
--
-- Note that no result is returned if some program crashes or fails to
-- terminate.
--
-- Example usage:
--
-- @
--   'testSelfInterpreterWith' eval selfInterpreter 'add'
--     ['fromNatural' 2, 'fromNatural' 2]
-- @

testSelfInterpreterWith
  :: (Exp -> Exp)  -- ^ Evaluation.
  -> Exp           -- ^ The self-interpreter.
  -> Exp           -- ^ The program to be evaluated (not in coded form).
  -> [Exp]         -- ^ The arguments (not in coded form).
  -> Bool
testSelfInterpreterWith eval selfInterpreter p args =
  runSelfInterpreter eval selfInterpreter p args
    ==
  Just (eval (foldl1 Apply (p : args)))

-- | Tests whether a purported implementation of evaluation and a
-- purported self-interpreter are correctly implemented.
--
-- The test is, of course, incomplete.

testSelfInterpreter
  :: (Exp -> Exp)  -- ^ Evaluation.
  -> Exp           -- ^ The self-interpreter.
  -> IO Bool
testSelfInterpreter eval selfInterpreter =
  isSuccess <$>
    quickCheckResult (\m ->
      (\n ->
        testSelfInterpreterWith eval selfInterpreter
          add [fromNatural (m `min` 5), fromNatural (n `min` 5)])
        .&&.
      testSelfInterpreterWith eval selfInterpreter
        add' [fromNatural m])
  where
  add' :: Exp
  add' = parse $ unlines
    [ "rec add = \\m. case m of"
    , "  { Zero()  -> \\n. n"
    , "  ; Succ(m) -> \\n. add m Succ(n)"
    , "  }"
    ]

------------------------------------------------------------------------
-- Internal tests

return []

-- | Runs all the internal tests.

tests :: IO Bool
tests = $quickCheckAll
