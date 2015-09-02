{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Chat.Server where

import           Control.Concurrent
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either
import           Data.Maybe
import           MarkdownCT
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Docs
import qualified System.Logging.Facade as Log

import           Chat.Api

chatApp :: MVar [(Person, Message)] -> Server ChatApi
chatApp mvar = apiDocs :<|> postMessage mvar :<|> getMessages mvar

apiDocs :: EitherT ServantErr IO Markdown
apiDocs = return . Markdown . markdown $ docs chatApi

postMessage :: MVar [(Person, Message)] -> Person -> Message -> EitherT ServantErr IO ()
postMessage mvar p msg = liftIO $ modifyMVar_ mvar $ \ messages ->
  return (messages ++ [(p, msg)])

getMessages :: MVar [(Person, Message)] -> Maybe Int -> EitherT ServantErr IO ([(Person, Message)], Int)
getMessages mvar (fromMaybe 0 -> offset) = do
  messages <- liftIO $ readMVar mvar
  return $ (, length messages) $ if offset >= 0
    then drop offset messages
    else reverse $ take (negate offset) $ reverse messages

main :: IO ()
main = do
  mvar <- newMVar []
  let port = 8087
      settings =
        setPort port $
        setBeforeMainLoop (Log.info ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings $ serve chatApi (chatApp mvar)
