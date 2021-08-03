
module Descent.TH where

import Data.Traversable

import Language.Haskell.TH

data DescentConfig = DescentConfig
  { recure :: [Name]
  , visit  :: [Name]
  }

generateDescentInstances :: DescentConfig -> Q [Dec]
generateDescentInstances DescentConfig { recure, visit } = do
  for recure \root -> do
    Just descent <- lookupTypeName "Descent"
    info <- reify root
    case info of
      TyConI (DataD _ _ _ _ cons _) -> do
        matches <- for cons ctors
        return
          $ InstanceD Nothing [] (AppT (ConT descent) (ConT root))
          [ FunD (mkName "descend")
            [Clause [arg "_leaf", arg "_branch"] (NormalB (LamCaseE matches)) []]
          ]
      i -> fail $ show i
  where
    arg = VarP . mkName
    var = VarE . mkName

    ctors :: Con -> Q Match
    ctors = \case
      NormalC   ctorName  args   -> matcher ctorName [ty | (_, ty) <- args]
      RecC      ctorName  args   -> matcher ctorName [ty | (_, _, ty) <- args]
      GadtC    [ctorName] args _ -> matcher ctorName [ty | (_, ty) <- args]
      RecGadtC [ctorName] args _ -> matcher ctorName [ty | (_, _, ty) <- args]
      InfixC (_, l) ctorName (_, r) -> matcher ctorName [l, r]
      c                             -> fail $ "not supported: " ++ show c

    matcher :: Name -> [Type] -> Q Match
    matcher ctor args = return $ Match pat body []
      where
        pat :: Pat
        pat = ConP ctor [VarP var' | (var', _) <- vars]

        body :: Body
        body
          = NormalB
          $ UInfixE (ConE ctor) (var "<$>")
          $ foldl1 (\a b -> UInfixE a (var "<*>") b) (map visitor' vars)

        visitor' :: (Name, Type) -> Exp
        visitor' (n, ty) = AppE (visitor ty) (VarE n)

        visitor :: Type -> Exp
        visitor (ConT ty)
          | ty `elem` recure = var "_branch"
          | ty `elem` visit  = var "_leaf"
          | otherwise        = var "pure"

        visitor (AppT _ x) = var "traverse" `AppE` visitor x
        visitor ty = error $ "not supported " ++ show ty

        vars = zip ([mkName ("a" ++ show i) | i <- [0 :: Int ..]]) args