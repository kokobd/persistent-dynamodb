{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Spec.Database.Persist.DynamoDB where

import qualified Amazonka
import qualified Amazonka.Auth as Amazonka
import qualified Amazonka.DynamoDB as Amazonka
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Generics.Labels ()
import Data.Generics.Product (field)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.UUID.V4 as UUID
import Database.Persist
import qualified Database.Persist.DynamoDB as DynamoDB
import Database.Persist.TH
import GHC.Generics (Generic)
import Hedgehog (MonadGen, property, (===))
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Property (forAllT)
import qualified Hedgehog.Range as Range
import qualified Network.Socket.Wait
import System.Environment (lookupEnv)
import System.IO (Handle, IOMode (WriteMode), openFile)
import qualified System.IO as IO
import qualified System.Process as Process
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.Hedgehog (testProperty)

share
  [mkPersist DynamoDB.defaultMkPersistSettings]
  [persistLowerCase|
User
  name Text
  email Text
  age Int
  deriving Show Eq
User2
  name Text
  email Text
  age Int
  Primary name email
  deriving Show Eq
|]

keyGen :: (MonadGen m, MonadIO m) => m (BackendKey DynamoDB.Backend)
keyGen = do
  uuid <- liftIO UUID.nextRandom
  pure $ DynamoDB.BackendKeyUUID uuid

userGen :: (MonadGen m) => m User
userGen = do
  userName <- Gen.text (Range.linear 1 10) Gen.unicode
  userEmail <- Gen.text (Range.linear 1 10) Gen.latin1
  userAge <- Gen.int (Range.linear 10 50)
  pure User {..}

user2Gen :: (MonadGen m) => m User2
user2Gen = do
  User {..} <- userGen
  pure User2 {user2Name = userName, user2Email = userEmail, user2Age = userAge}

migrateTables :: (MonadIO m) => Amazonka.Env -> m ()
migrateTables env = liftIO . Amazonka.runResourceT $ do
  void . Amazonka.send env $
    Amazonka.newCreateTable
      "user"
      (Amazonka.newKeySchemaElement "id" Amazonka.KeyType_HASH :| [])
      & field @"attributeDefinitions" .~ [Amazonka.newAttributeDefinition "id" Amazonka.ScalarAttributeType_B]
      & field @"billingMode" ?~ Amazonka.BillingMode_PAY_PER_REQUEST
  void . Amazonka.send env $
    Amazonka.newCreateTable
      "user2"
      ( Amazonka.newKeySchemaElement "name" Amazonka.KeyType_HASH
          :| [ Amazonka.newKeySchemaElement "email" Amazonka.KeyType_RANGE
             ]
      )
      & field @"attributeDefinitions"
        .~ [ Amazonka.newAttributeDefinition "name" Amazonka.ScalarAttributeType_S,
             Amazonka.newAttributeDefinition "email" Amazonka.ScalarAttributeType_S
           ]
      & field @"billingMode" ?~ Amazonka.BillingMode_PAY_PER_REQUEST
  pure ()

data Resources = Resources
  { backend :: DynamoDB.Backend,
    ddbProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, Process.ProcessHandle)
  }
  deriving (Generic)

initResources :: IO Resources
initResources = do
  nullFile <- openFile "/dev/null" WriteMode
  dynamoDBPort <- read @Int . fromMaybe "8000" <$> lookupEnv "DYNAMODB_PORT"
  ddbProcess <-
    Process.createProcess
      (Process.shell $ "dynamodb -inMemory -port " <> show dynamoDBPort)
        { Process.std_out = Process.UseHandle nullFile,
          Process.create_group = True,
          Process.close_fds = True
        }
  Network.Socket.Wait.wait "localhost" dynamoDBPort
  logger <- Amazonka.newLogger Amazonka.Info IO.stdout
  env <- Amazonka.newEnv $ pure . Amazonka.fromKeys "fakeKeyId" "fakeSecretAccessKey"
  let env' = env {Amazonka.logger, Amazonka.overrides = Amazonka.setEndpoint False "localhost" dynamoDBPort}
  migrateTables env'
  pure Resources {backend = DynamoDB.newBackend env', ddbProcess}

freeResources :: Resources -> IO ()
freeResources Resources {ddbProcess} = do
  let handle = view _4 ddbProcess
  Process.interruptProcessGroupOf handle
  void $ Process.waitForProcess handle
  Process.cleanupProcess ddbProcess

test_integration :: TestTree
test_integration =
  withResource initResources freeResources $ \getResources ->
    testGroup
      "integration tests"
      [ testProperty "insert and get with default primary key" $
          property $ do
            (UserKey -> key, user) <- forAllT $ (,) <$> keyGen <*> userGen
            user' <- runDBActions getResources $ do
              insertKey key user
              get key
            Just user === user'
            pure (),
        testProperty "insert and get with custom primary key" $
          property $ do
            user2 <- forAllT user2Gen
            let key = User2Key (user2Name user2) (user2Email user2)
            user' <- runDBActions getResources $ do
              insertKey key user2
              get key
            Just user2 === user'
      ]
  where
    runDBActions :: (MonadIO m) => IO Resources -> ReaderT DynamoDB.Backend m a -> m a
    runDBActions getResources actions = liftIO (view #backend <$> getResources) >>= runReaderT actions
