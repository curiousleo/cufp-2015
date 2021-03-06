
---------------------------------------
-- T13: Web Programming with Servant --
---------------------------------------

-- # Outline

-- 1) Introduction
-- 2) Coding

-- # Preparations for 2)

-- git clone git@github.com:haskell-servant/cufp-2015
-- (or: git clone https://github.com/haskell-servant/cufp-2015)
-- cd cufp-2015
-- ./setup.sh

-- # Introduction

-- ## How this introduction works

-- It's a compilable Haskell module. Here's proof:

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Part1 where

import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.List
import           Data.Text hiding (filter, isInfixOf)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp
import           Servant
import           Servant.API.ContentTypes ()

-- And it includes doctest examples.

-- $ >>> 1 + 2 :: Int
-- 3

-- This hopefully allows to make this
-- introduction very interactive.
-- Questions and comments welcome.

-- ## API DSL

-- The central idea of servant is that APIs can
-- be expressed as types (combined from existing types).

type SimpleGet = Get '[JSON] [Int]

type SimplePost = Post '[JSON] String

-- Note that so far these are just types that describe APIs
-- There is no executable piece of code yet.


-- There's two important infix operators (on the type level):

-- 1) The sequential operator, called sub, aka bird-face: :>

type SimplePath = "foo" :> Get '[JSON] [Int]

-- 2) The alternative operator: :<|>

type SimpleAlternative =
       Get '[JSON] [Int]
  :<|> Post '[JSON] String


-- Of course, these operators can be combined:

type Combined =
       "list" :> Get '[JSON] [Int]
  :<|> "list" :> "poke" :> Post '[JSON] String

-- Again, those are just types. But now that we have a
-- high-level description of APIs (at the type level)
-- we can interpret in multiple ways, e.g. for:
-- - writing servers,
-- - obtaining client functions,
-- - generate documentation,
-- - etc.


-- ## Writing a Server to an API

-- Servant allows you to map type-level APIs to types of web-handlers
-- using type families.

type Simple = Get '[JSON] [Int]

simpleServer :: Server Simple
simpleServer = error "nyi"

-- $ >>> :kind! Server Simple
-- ...
-- = EitherT ServantErr IO [Int]

-- `serve` let's us create wai `Application`s for all types that
-- have a `HasServer` instance.

simpleApp :: Application
simpleApp = serve simpleProxy simpleServer

simpleProxy :: Proxy Simple
simpleProxy = Proxy

simpleRun :: IO ()
simpleRun = Warp.run 8080 simpleApp


-- We can change the type. We can e.g.
-- - change the return type,
-- - add an endpoint,
-- - add a path.

-- This is one of the central ideas of Servant:
-- - It allows to specify APIs at the type-level,
-- - it offers -- in the case of the server
--   interpretation -- a type family that computes the type of the
--   server that the user has to implement in order to conform to
--   the specified API.

-- The typesystem will always make sure that our server always implements the
-- specified API.


-- # Further Combinators

-- `DemoReqBody` is just used to demonstrate the principle of so-called
-- combinators in servant.

data DemoReqBody a

-- It's an uninhabited type. It can be used in API specifications.

type SimpleBody = DemoReqBody Text :> Get '[PlainText] Text

-- To be able to implement a `Server` for this API we need a `HasServer`
-- instance. This instance -- together with its associated type (`ServerT`) --
-- gives `DemoReqBody` its meaning. (This is also called an interpretation
-- of the type-level term.)

instance HasServer api => HasServer (DemoReqBody a :> api) where
  type ServerT (DemoReqBody a :> api) m = a -> ServerT api m
  route = error "DemoReqBody.route: nyi"

-- $ >>> :kind! Server SimpleBody
-- ...
-- = Text -> EitherT ServantErr IO Text

simpleBodyServer :: Server SimpleBody
simpleBodyServer s = return $ Data.Text.reverse s

simpleBodyRun :: IO ()
simpleBodyRun =
  Warp.run 8080 (serve (Proxy :: Proxy SimpleBody) simpleBodyServer)

-- It's not hard to write your own combinators. But servant already provides
-- a bunch of common combinators, e.g. `ReqBody`.

-- $ >>> :kind! Server (ReqBody '[JSON] Text :> Post '[JSON] ())
-- ...
-- = Text -> EitherT ServantErr IO ()


-- We already know the path combinator. It doesn't change the
-- type of the server.

-- $ >>> :kind! Server ("path" :> Get '[JSON] ())
-- ...
-- = EitherT ServantErr IO ()


-- The `QueryParam` combinator allows to retrieve query parameters.

-- $ >>> :kind! Server (QueryParam "key" Text :> Get '[JSON] ())
-- ...
-- = Maybe Text -> EitherT ServantErr IO ()


-- There are two other variants for query parameters.

-- $ >>> :kind! Server (QueryParams "keys" Text :> Get '[JSON] ())
-- ...
-- = [Text] -> EitherT ServantErr IO ()

-- $ >>> :kind! Server (QueryFlag "flag" :> Get '[JSON] ())
-- ...
-- = Bool -> EitherT ServantErr IO ()


-- There's the `Capture` combinator to extract values from the
-- url path.

-- $ >>> :kind! Server (Capture "flag" Text :> Get '[JSON] ())
-- ...
-- = Text -> EitherT ServantErr IO ()


-- # A Slightly Bigger Example

-- Our domain:

data Person
  = Person {
    name :: String,
    age :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON)

alice :: Person
alice = Person "alice" 1

bob :: Person
bob = Person "bob" 2

allPersons :: [Person]
allPersons = [alice, bob]

-- Here's our API:

type ExampleApi =
  "person" :> (
         Get '[JSON] [Person]
    :<|> "get" :> Capture "name" String :> Get '[JSON] Person
    :<|> "search" :> QueryParam "q" String :> Get '[JSON] [Person]
  )

exampleProxy :: Proxy ExampleApi
exampleProxy = Proxy

-- Our server implementation:

exampleRun :: IO ()
exampleRun = Warp.run 8080 (serve exampleProxy exampleServer)

exampleServer :: Server ExampleApi
exampleServer =
       return allPersons
  :<|> getPerson
  :<|> searchPerson

-- Handler implementations:

getPerson :: String -> EitherT ServantErr IO Person
getPerson n = case filter (\ p -> name p == n) allPersons of
  [p] -> return p
  _ -> left err404

searchPerson :: Maybe String -> EitherT ServantErr IO [Person]
searchPerson (Just query) = return $
  filter (\ person -> query `isInfixOf` name person) allPersons
searchPerson Nothing = left $ err400


-- One last thought:

-- Haskell's normal means of abstracting types can be used to structure API
-- specifications:

type MainApi =
       PublicApi
  :<|> AdminApi

type PublicApi = Get '[JSON] ()
type AdminApi = Get '[JSON] ()

-- # Coding
