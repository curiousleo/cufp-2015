{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chat.Api where

import           Data.Aeson
import           Data.String
import           GHC.Generics
import           MarkdownCT
import           Servant
import           Servant.Docs

newtype Person = Person { name :: String }
    deriving (Eq, Show, Generic, FromText, ToText, ToJSON, FromJSON)

newtype Message = Message { unMessage :: String }
    deriving (Eq, Show, Generic, IsString, ToJSON, FromJSON)

type ChatApi =
       "docs" :> Get '[Markdown] Markdown
  :<|> "message" :> Capture "person" Person :> ReqBody '[JSON] Message :> Post '[JSON] ()
  :<|> "massages" :> QueryParam "offset" Int :> Get '[JSON] ([(Person, Message)], Int)

instance ToSample Message Message where
    toSample _ = Just $ Message "hi, this is a message"

instance ToSample ([Message], Int) ([Message], Int) where
  toSample Proxy = Just (messages, length messages)
    where
      messages = map snd $ toSamples (Proxy :: Proxy Message)

instance ToSample Int Int where
    toSample _ = Just 42

instance ToSample [(Person, Message)] [(Person, Message)] where
    toSample _ = Just [(Person "julian", Message "hi")]

instance ToSample ([(Person, Message)], Int) ([(Person, Message)], Int) where
    toSample _ = Just ([(Person "julian", Message "hi")], 1)

instance ToCapture (Capture "person" Person) where
    toCapture _ = DocCapture "person" "the person's name"


chatApi :: Proxy ChatApi
chatApi = Proxy
