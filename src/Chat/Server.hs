{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE ViewPatterns #-}

module Chat.Server where

import           Servant
import           Servant.Docs
import           Network.Wai.Handler.Warp
import qualified System.Logging.Facade as Log
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either
import           Data.Maybe

import           Chat.Api
import           MarkdownCT

postMessage :: MVar [(Person, Message)] -> Person -> Message -> EitherT ServantErr IO ()
postMessage mvar person message = liftIO $ modifyMVar_ mvar go
  where
    go :: [(Person, Message)] -> IO [(Person, Message)]
    go xs = return $ xs ++ [(person, message)]

getDocs :: EitherT ServantErr IO Markdown
getDocs = right $ Markdown $ markdown $ docs chatApi

getMessages :: MVar [(Person, Message)] -> Maybe Int -> EitherT ServantErr IO ([(Person, Message)], Int)
getMessages mvar (fromMaybe 0 -> offset) = do
  messages <- liftIO $ takeMVar mvar
  right (drop offset messages, length messages)

chatApp :: MVar [(Person, Message)] -> Server ChatApi
chatApp mvar = getDocs
     :<|> postMessage mvar
     :<|> getMessages mvar

main :: IO ()
main = do
  let port = 8087
      settings =
        setPort port $
        setBeforeMainLoop (Log.info ("listening on port " ++ show port)) $
        defaultSettings
  mvar <- newMVar []
  runSettings settings $ serve chatApi (chatApp mvar)

runServer :: Port -> IO ()
runServer = error "not yet implemented"
