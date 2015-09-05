-- The untyped lambda calculus. This will form the core business logic for our
-- application.
{-# LANGUAGE DeriveGeneric #-}
module Lambda.Logic where

import           Data.Aeson
import           GHC.Generics

data Term = Var String
          | Lambda String Term
          | App Term Term
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON Term
instance FromJSON Term

-- A pretty-printer that might help keep you sane.
pretty :: Term -> String
pretty (Var x) = x
pretty (Lambda x t) = "(\\" ++ x ++ " -> " ++ pretty t ++ ")"
pretty (App x t) = "(" ++ pretty x ++ ") " ++ pretty t

-- [TASK] Implement eval for our lambda calculus
substitute :: Term -> String -> Term -> Term
substitute (Var var) name term | var == name = term
substitute (Lambda lam def) name term | lam /= name = Lambda lam (substitute def name term)
substitute (App f arg) name term = App (substitute f name term) (substitute arg name term)
substitute x _ _ = x

evaluate :: Term -> Term
evaluate (App (Lambda lam term) arg) = substitute (evaluate arg) lam term
evaluate (App _                 _  ) = error "must apply to lambda"
evaluate term                        = term
