module AST where

import Data.String (IsString (..))

import Descent (Descent(..), DescentConfig (..), generateDescentInstances)

-- An example program type.

data Prog
  = Var   NameVar
  | Lam   NameDecl Prog
  | App   Prog Prog
  | Let   Decl Prog
  | Match Prog [Alt]
  | Const Constant
  | New   [Decl]
  | Ctor  NameCtor Int
  | Mark  String Prog
  deriving stock (Eq, Ord, Show)

instance IsString Prog where fromString = Var . fromString

-- | I recommend separating types for usage and declaration of some entity.
--
--   This will greatly help when working with this entity.
--
--   So, typenames, for instance, should have their own TName/TNameUsed newtypes.
--
data Name = Name { unName :: String, index :: Int } deriving stock (Eq, Ord)

instance Show Name where
  show (Name s (-1)) = s
  show (Name s   n)  = "#" ++ show n

instance IsString Name where fromString = flip Name (-1)

newtype NameVar  = NameVar  { unNameVar  :: Name } deriving newtype (Eq, Ord, Show, IsString)
newtype NameDecl = NameDecl { unNameDecl :: Name } deriving newtype (Eq, Ord, Show, IsString)
newtype NameCtor = NameCtor { unNameCtor :: Name } deriving newtype (Eq, Ord, Show, IsString)
newtype NameLet  = NameLet  { unNameLet  :: Name } deriving newtype (Eq, Ord, Show, IsString)

data Alt
  = Alt Pat Prog
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
  = IsBind    NameVar Pat
  | IsCapture NameDecl
  deriving stock (Eq, Ord, Show)

data Constant
  = I Integer
  | F Double
  | S String
  deriving stock (Eq, Ord, Show)

generateDescentInstances DescentConfig
  { recure = [''Prog, ''Alt, ''Decl, ''Pat, ''IsDecl]
  , visit  = [''NameVar, ''NameDecl, ''NameCtor, ''NameLet, ''Constant]
  }
