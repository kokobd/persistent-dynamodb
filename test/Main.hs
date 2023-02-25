{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

module Main (main) where

import qualified Amazonka
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.UUID as UUID
import Database.Persist
import qualified Database.Persist.DynamoDB as DynamoDB
import Database.Persist.TH
import qualified System.IO as IO

share
  [mkPersist DynamoDB.defaultMkPersistSettings]
  [persistLowerCase|
User
  name Text
  email Text
  age Int
  deriving Show
|]

main :: IO ()
main = do
  logger <- Amazonka.newLogger Amazonka.Info IO.stdout
  env <- Amazonka.newEnv Amazonka.discover
  let env' = env {Amazonka.logger}
  let backend = DynamoDB.newBackend env'
  flip runReaderT backend $ do
    insertKey exampleKey $ User "Kobayashi" "123@456.com" 21
    record <- get @DynamoDB.Backend @User exampleKey
    liftIO $ print record
    pure ()

exampleKey :: Key User
exampleKey = UserKey $ DynamoDB.BackendKeyUUID exampleUUID

exampleUUID :: UUID.UUID
exampleUUID = fromJust $ UUID.fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c"
