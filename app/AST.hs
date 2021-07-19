module AST where

import Data.String (IsString (..))

import Descent (Descent(..))

-- An example program type.

data Prog
  = Var   Name
  | Lam   NameDecl Prog
  | App   Prog Prog
  | Let   [Decl] Prog
  | Match Prog [Alt]
  | Const Constant
  | New   [Decl]
  | Ctor  NameCtor Int
  | Mark  String Prog
  deriving stock (Eq, Ord, Show)

instance IsString Prog where
  fromString = Var . fromString

instance Descent Prog where
  --      +--------- call on any `Typeable`
  --      |    +---- call on `Descent` instances
  --      v    v
  descend leaf branch = \case
    Var   n       -> Var   <$>          leaf   n
    Lam   n  b    -> Lam   <$>          leaf   n  <*> branch b
    App   f  x    -> App   <$>          branch f  <*> branch x
    Let   ds b    -> Let   <$> traverse branch ds <*> branch b
    Match o  pats -> Match <$>          branch o  <*> traverse branch pats
    Const c       -> Const <$>          leaf   c
    New   ds      -> New   <$> traverse branch ds
    Ctor  n  ar   -> Ctor  <$>          leaf   n  <*> pure ar
    Mark  s  prog -> Mark  <$>          pure   s  <*> branch prog

-- | I recommend separating types for usage and declaration of some entity.
--
--   This will greatly help when working with this entity.
--
--   So, typenames, for instance, should have their own TName/TNameUsed newtypes.
--
newtype Name     = Name     {unName     :: String } deriving stock (Eq, Ord) deriving newtype Show
newtype NameDecl = NameDecl {unNameDecl :: String } deriving stock (Eq, Ord) deriving newtype Show
newtype NameCtor = NameCtor {unNameCtor :: String } deriving stock (Eq, Ord) deriving newtype Show

instance IsString Name     where fromString = Name
instance IsString NameDecl where fromString = NameDecl
instance IsString NameCtor where fromString = NameCtor

data Alt = Alt Pat Prog
  deriving stock (Eq, Ord, Show)

instance Descent Alt where
  descend _ branch = \case
    Alt p b -> Alt <$> branch p <*> branch b

data Decl
  = Bind { name :: NameDecl, body :: Prog }
  deriving stock (Eq, Ord, Show)

instance Descent Decl where
  descend leaf branch = \case
    Bind n b -> Bind <$> leaf n <*> branch b

data Pat
  = IsCtor  NameCtor [Pat]
  | IsVar   NameDecl
  | IsConst Constant
  | IsObj   [IsDecl]
  | IsView  Prog Pat
  | As      Pat Pat
  deriving stock (Eq, Ord, Show)

instance Descent Pat where
  descend leaf branch = \case
    IsCtor  n pats -> IsCtor  <$> leaf n <*> traverse branch pats
    IsVar   n      -> IsVar   <$> leaf n
    IsConst c      -> IsConst <$> leaf c
    IsObj   ds     -> IsObj   <$> traverse branch ds
    IsView  p b    -> IsView  <$> branch p <*> branch b
    As      l r    -> As      <$> branch l <*> branch r

data IsDecl
  = IsBind    Name Pat
  | IsCapture NameDecl
  deriving stock (Eq, Ord, Show)

instance Descent IsDecl where
  descend leaf branch = \case
    IsBind    n p -> IsBind    <$> leaf n <*> branch p
    IsCapture n   -> IsCapture <$> leaf n

data Constant
  = I Integer
  | F Double
  | S String
  deriving stock (Eq, Ord, Show)
