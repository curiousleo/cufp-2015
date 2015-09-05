module Lambda.Client where

-- import           Control.Monad
import           Control.Monad.Trans.Either
import           Servant.API
import           Servant.Client

import           Lambda.Api
import           MarkdownCT

getDocs :: EitherT ServantError IO Markdown
getLambda :: String -> EitherT ServantError IO Term
getVar :: String -> Term -> EitherT ServantError IO Term
getApp :: (Term, Term) -> EitherT ServantError IO Term
getEval :: Term -> EitherT ServantError IO Term

(getDocs :<|> getLambda :<|> getVar :<|> getApp :<|> getEval) = client lambdaApi (BaseUrl Http "localhost" 8081)

remoteEval :: Term -> Term
remoteEval term = undefined
