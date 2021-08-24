
import Control.Applicative ((<|>))
import Control.Monad.State

import AST
import Descent

-- Test subject.
--
ast :: Prog
ast
  = Let (Bind "id"    $ Lam "a" "a")
  $ Let (Bind "const" $ Lam "a" $ Lam "b" "a")
  $ Match (App "id" (Const $ I 1))
      [ Alt (IsVar "a" `As` IsVar "b")
          (App "const" (App (App "plus" "a") "b"))
      ]

-- Our working state. Is a list of stack frames.
--
type Scope = [[String]]

-- Our working monad.
--
-- The state consists of stack frames of names.
--
type ScopeM = StateT Scope IO

-- Run the `ScopeM`.
--
runScopeM :: ScopeM r -> IO r
runScopeM
  = flip evalStateT []

-- Find the current depth of the name and attach it.
rename :: Name -> ScopeM Name
rename name = do
  scope <- get
  case find (unName name) scope of
    Just index -> return $ addIndex name index
    Nothing    -> return name

-- Find the current depth of the name.
find :: String -> Scope -> Maybe Int
find _ [] = Nothing
find n (frame : rest) = case find' n frame of
  Nothing -> (length frame +) <$> find n rest
  Just n  -> return n
  where
    find' n (n' : rest')
      | n == n' = return 0
      | otherwise = (1 +) <$> find' n rest'

    find' _ [] = Nothing


-- Add index to the name.
--
addIndex :: Name -> Int -> Name
addIndex (Name name _) index = Name name index

-- Preparations before entering a `Prog`-ram.
--
-- If the name ("a") is bound, convert it to "a'N", where N is its depth in stack.
--
useName :: Transform ScopeM
useName = pack
  [ Entering `with_` \case
      Lam (NameDecl n) _ -> modify ([unName n] :)
      _                  -> return ()

  , Entering `with_` \case
      Alt {} -> modify ([] :)

  , Entering `with_` \case
      IsVar (NameDecl n) -> modify \(top : s) -> (unName n : top) : s
      _                  -> return ()

  , coercing @NameVar  Inside rename
  , coercing @NameDecl Inside rename

  -- Small hack. We assign depth the name would take (but it didn't).
  , Inside `with` \(NameLet name) -> do
      return $ NameLet (addIndex name 0)

  , Leaving `with_` \case
      Lam {} -> dropFrame
      Let {} -> dropFrame
      _      -> return ()

  , Leaving `with_` \case
      Bind (NameLet n) _ -> modify ([unName n] :)

  , Leaving `with_` \case
      Alt {} -> dropFrame
  ]
  where
    dropFrame = modify tail

main :: IO ()
main = do
  print ast
  -- assign initial De Brujin indices to names.
  ast' <- runScopeM $ runTransform (descending @Prog useName) ast
  print "----"
  print ast'
