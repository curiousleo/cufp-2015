{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Lambda.Server where

import           Control.Monad.Trans.Either
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           System.Logging.Facade as Log
import           Servant
import           Servant.Docs

import           Lambda.Api
import           Lambda.Logic

import           MarkdownCT

-- [TASK] Implement this server
getDocs :: EitherT ServantErr IO Markdown
getDocs = right $ Markdown $ markdown $ docs lambdaApi

getVar :: String -> EitherT ServantErr IO Term
getVar var = return $ Var var

getLambda :: String -> Term -> EitherT ServantErr IO Term
getLambda lam def = return $ Lambda lam def

getApp :: (Term, Term) -> EitherT ServantErr IO Term
getApp (f, arg) = return $ App f arg

getEval :: Term -> EitherT ServantErr IO Term
getEval term = return $ evaluate term

lambdaServer :: Server LambdaApi
lambdaServer = getDocs :<|> getVar :<|> getLambda :<|> getApp :<|> getEval

runServer :: Port -> IO ()
runServer p = do
  let settings =
        setPort p $
        setBeforeMainLoop
          (Log.info ("listening on port " ++ show p)) $
        defaultSettings
  runSettings settings $ logStdoutDev $ serve lambdaApi lambdaServer
