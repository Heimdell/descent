module AST where

import Data.String (IsString (..))

import Descent (Descent(..), DescentConfig (..), generateDescentInstances)

-- An example program type.

data Prog
  = Var   Name
  | Lam   NameDecl Prog
  | App   Prog Prog
  | Let   Decl Prog
  | Match Prog [Alt]
  | Const Constant
  | New   [Decl]
  | Ctor  NameCtor Int
  | Mark  String Prog
  deriving stock (Eq, Ord, Show)

instance IsString Prog where
  fromString = Var . fromString

-- | I recommend separating types for usage and declaration of some entity.
--
--   This will greatly help when working with this entity.
--
--   So, typenames, for instance, should have their own TName/TNameUsed newtypes.
--
newtype Name     = Name     { unName     :: String } deriving stock (Eq, Ord) deriving newtype Show
newtype NameDecl = NameDecl { unNameDecl :: String } deriving stock (Eq, Ord) deriving newtype Show
newtype NameCtor = NameCtor { unNameCtor :: String } deriving stock (Eq, Ord) deriving newtype Show
newtype NameLet  = NameLet  { unNameLet  :: String } deriving stock (Eq, Ord) deriving newtype Show

instance IsString Name     where fromString = Name
instance IsString NameDecl where fromString = NameDecl
instance IsString NameCtor where fromString = NameCtor
instance IsString NameLet  where fromString = NameLet

data Alt = Alt Pat Prog
  deriving stock (Eq, Ord, Show)

data Decl
  = Bind { name :: NameLet, body :: Prog }
  deriving stock (Eq, Ord, Show)

data Pat
  = IsCtor  NameCtor [Pat]
  | IsVar   NameDecl
  | IsConst Constant
  | IsObj   [IsDecl]
  | IsView  Prog Pat
  | As      Pat Pat
  deriving stock (Eq, Ord, Show)

data IsDecl
  = IsBind    Name Pat
  | IsCapture NameDecl
  deriving stock (Eq, Ord, Show)

data Constant
  = I Integer
  | F Double
  | S String
  deriving stock (Eq, Ord, Show)

generateDescentInstances DescentConfig
  { recure = [''Prog, ''Alt, ''Decl, ''Pat, ''IsDecl]
  , visit  = [''Name, ''NameDecl, ''NameCtor, ''NameLet, ''Constant]
  }