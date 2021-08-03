
module Descent.Example where

import Descent

data Foo where
  MkFoo, A :: Int -> String -> Foo

class Bar b

generateDescentInstances DescentConfig
  { recure = [''Foo]
  , visit  = [''Int]
  }
