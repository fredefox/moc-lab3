

module AbsChi where

-- Haskell module generated by the BNF converter




newtype Constructor = Constructor String
  deriving (Eq, Ord, Show, Read)
newtype Variable = Variable String deriving (Eq, Ord, Show, Read)
data Exp
    = Apply Exp Exp
    | Lambda Variable Exp
    | Case Exp [Br]
    | Rec Variable Exp
    | Var Variable
    | Const Constructor [Exp]
  deriving (Eq, Ord, Show, Read)

data Br = Branch Constructor [Variable] Exp
  deriving (Eq, Ord, Show, Read)

