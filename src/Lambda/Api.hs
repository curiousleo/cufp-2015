-- The description of the API, including the datatypes (and instances) that
-- participate in that API.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lambda.Api (
  module Lambda.Api,
  Term,
 ) where

import           Data.Proxy
import           Servant.API
import           Servant.Docs

import           Lambda.Logic
import           MarkdownCT

type LambdaApi =
       Docs

  :<|> Var
  :<|> Lambda
  :<|> App
  :<|> Eval

lambdaApi :: Proxy LambdaApi
lambdaApi = Proxy

type Docs = "docs" :> Get '[Markdown] Markdown

type Var = "var" :> Capture "var" String :> Get '[JSON] Term

type Lambda = "lambda" :> Capture "parameter" String :> ReqBody '[JSON] Term :> Get '[JSON] Term

type App = "app" :> ReqBody '[JSON] (Term, Term) :> Get '[JSON] Term

type Eval = "eval" :> ReqBody '[JSON] Term :> Get '[JSON] Term

-- * Instances for documentation.

instance (ToSample a a, ToSample b b) => ToSample (a, b) (a, b) where
  toSample Proxy = (,) <$>
    (toSample (Proxy :: Proxy a)) <*>
    (toSample (Proxy :: Proxy b))

instance ToSample Term Term where
  toSample Proxy = Just (Lambda "x" (Var "x"))

instance ToCapture (Capture "parameter" String) where
  toCapture Proxy = DocCapture "parameter" "name of the parameter of the lambda abstraction"

instance ToCapture (Capture "var" String) where
  toCapture Proxy = DocCapture "var" "variable name"
