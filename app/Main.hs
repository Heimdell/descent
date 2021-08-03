
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

-- If the name ("a") is bound, convert it to "a'N", where N is its depth in stack.
--
useName :: Transform ScopeM
useName = pack
  [ one \(NameVar name) -> do
      name' <- rename name
      get >>= \s ->
        lift $ print ("Name", name, "=>", name', "in", s)
      return $ NameVar name'

  , one \(NameDecl name) -> do
      name' <- rename name
      -- lift $ print ("Decl", name, "->", name')
      return $ NameDecl name'

  -- Small hack. We assign depth the name would take (but it didn't).
  , one \(NameLet name) -> do
      let name' = addIndex name 0
      -- lift $ print ("Let", name, "->", name')
      return $ NameLet name'
  ]

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
enter :: Transform ScopeM
enter = pack
  [ one $ sideEffect \case

      -- In case of lambda, insert a stack frame of 1 variable.
      --
      Lam (NameDecl n) _ -> do
        -- lift $ putStrLn "Enter lambda"
        modify ([unName n] :)

      _ -> do
        return ()

  , one $ sideEffect \case

      -- In case of alt, insert an empty stack frame.
      --
      Alt {} -> do
        modify ([] :)
        -- lift $ putStrLn "Enter alt"

  , one $ sideEffect \case

      -- In case of var-pattern, insert an variable into the top stack frame.
      --
      IsVar (NameDecl n) -> do
        -- lift $ putStrLn "Enter isVar"
        modify \(top : s) -> (unName n : top) : s

      _ -> do
        return ()
  ]

-- Cleanouts. We drop top frame here, if applicable.
--
leave :: Transform ScopeM
leave = pack
  [ one $ sideEffect \case
      Lam {} -> dropFrame "lambda"
      Let {} -> dropFrame "let"
      _      -> return ()

  , one $ sideEffect \case
      Bind (NameLet n) _ -> do
        -- lift $ putStrLn $ "push " ++ show n
        modify ([unName n] :)

  , one $ sideEffect \case
      Alt {} -> dropFrame "alt"
  ]
  where
    dropFrame what = do
      -- lift $ putStrLn $ "Leave " ++ what
      modify tail

main :: IO ()
main = do
  print ast
  -- assign initial De Brujin indices to names.
  ast' <- runScopeM $ runTransform (descending @Prog enter leave useName) ast
  print "----"
  print ast'
