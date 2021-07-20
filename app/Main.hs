
import Control.Applicative ((<|>))
import Control.Monad.State
import Control.Monad.Cont

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
type Scope = [[(String, Int)]]

-- Our working monad. `ContT` here is so we can restore state.
--
-- The state consists of scope and a depth counter for names.
--
type ScopeM = StateT (Scope, Int) IO

-- Run the `ScopeM`.
--
runScopeM :: ScopeM r -> IO r
runScopeM
  = flip evalStateT ([], 0)

-- If the name ("a") is bound, convert it to "a'N", where N is its depth in stack.
--
useName :: Transform ScopeM
useName = pack
  [ one \(Name name) -> do
      name' <- rename name
      lift $ print ("Name", name, "=>", name')
      return $ Name name'

  , one \(NameDecl name) -> do
      name' <- rename name
      lift $ print ("Decl", name, "->", name')
      return $ NameDecl name'

  -- Small hack. We assign depth the name would take (but it didn't).
  , one \(NameLet name) -> do
      (_, counter) <- get
      let name' = addIndex name counter
      lift $ print ("Let", name, "->", name')
      return $ NameLet name'
  ]

rename :: String -> ScopeM String
rename name = do
  (scope, counter) <- get
  case find name scope of
    Just index -> return $ addIndex name index
    Nothing    -> return name

find :: String -> Scope -> Maybe Int
find _ [] = Nothing
find n (frame : rest) = lookup n frame <|> find n rest

-- Add index to the name.
--
addIndex :: String -> Int -> String
addIndex name index = name ++ "'" ++ show index

-- Preparations before entering a `Prog`-ram.
--
enter :: Transform ScopeM
enter = pack
  [ one $ sideEffect \case

      -- In case of lambda, insert a stack frame of 1 variable.
      --
      Lam (NameDecl n) b -> do
        lift $ putStrLn "Enter lambda"
        (scope, counter) <- get
        put ([(n, counter)] : scope, counter)

      _ -> do
        return ()

  , one $ sideEffect \case

      -- In case of alt, insert an empty stack frame.
      --
      Alt pat body -> do
        lift $ putStrLn "Enter alt"
        (scope, counter) <- get
        put ([] : scope, counter)

  , one $ sideEffect \case

      -- In case of var-pattern, insert an variable into the top stack frame.
      --
      IsVar (NameDecl n) -> do
        lift $ putStrLn "Enter isVar"
        get >>= \(frame : scope, counter) -> do
          put (((n, counter) : frame) : scope, counter + 1)

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
      Bind (NameLet n) b -> do
        (scope, counter) <- get
        put ([(n, counter)] : scope, counter + 1)

  , one $ sideEffect \case
      Alt {} -> dropFrame "alt"
  ]
  where
    dropFrame what = do
      lift $ putStrLn $ "Leave " ++ what
      (scope, ix) <- get
      put (tail scope, ix)

main = do
  print ast
  ast' <- runScopeM $ runTransform (descending @Prog enter leave useName) ast
  print ast'
