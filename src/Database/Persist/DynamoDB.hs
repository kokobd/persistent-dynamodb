{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Database.Persist.DynamoDB
  ( Backend,
    BackendKey (BackendKeyUUID),
    defaultMkPersistSettings,
    newBackend,
    Error (..),
  )
where

import qualified Amazonka
import qualified Amazonka.DynamoDB as Amazonka
import Control.Exception (Exception, throwIO)
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (second)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import Data.Generics.Product (field)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (forM)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Database.Persist
import Database.Persist.Sql (PersistFieldSql (sqlType))
import Database.Persist.TH (MkPersistSettings, mkPersistSettings)
import GHC.Generics (Generic)
import Language.Haskell.TH (Type (ConT))
import Numeric (showFFloat)
import Text.Read (readEither)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)
import Web.PathPieces (PathPiece (..))

newtype Backend = Backend {env :: Amazonka.Env}
  deriving stock (Generic)

newBackend :: Amazonka.Env -> Backend
newBackend = Backend

instance PersistCore Backend where
  newtype BackendKey Backend = BackendKeyUUID UUID
    deriving newtype (Show, Read, Eq, Ord, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData)

defaultMkPersistSettings :: MkPersistSettings
defaultMkPersistSettings = mkPersistSettings (ConT ''Backend)

instance PersistField (BackendKey Backend) where
  toPersistValue :: BackendKey Backend -> PersistValue
  toPersistValue (BackendKeyUUID uuid) = PersistByteString . LBS.toStrict . Binary.encode $ uuid

  fromPersistValue :: PersistValue -> Either Text (BackendKey Backend)
  fromPersistValue (PersistByteString bs) =
    bimap (\(_, _, msg) -> T.pack msg) (\(_, _, x) -> BackendKeyUUID x) $
      Binary.decodeOrFail (LBS.fromStrict bs)
  fromPersistValue _ = Left "only bytesring is supported"

instance PersistFieldSql (BackendKey Backend) where
  sqlType _ = SqlBlob

instance PathPiece (BackendKey Backend) where
  fromPathPiece txt = BackendKeyUUID <$> UUID.fromText txt
  toPathPiece (BackendKeyUUID uuid) = UUID.toText uuid

instance HasPersistBackend Backend where
  type BaseBackend Backend = Backend
  persistBackend = id

data Error
  = EncodeFailure Text
  | DecodeFailure Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Exception)

encodeValue :: PersistValue -> Amazonka.AttributeValue
encodeValue (PersistText v) = Amazonka.S v
encodeValue (PersistByteString v) = Amazonka.B (Amazonka.Base64 v)
encodeValue (PersistInt64 v) = Amazonka.N . T.pack $ show v
encodeValue (PersistDouble v) = Amazonka.N . T.pack $ showFFloat Nothing v ""
encodeValue (PersistRational v) = Amazonka.S . T.pack $ show v
encodeValue (PersistBool v) = Amazonka.BOOL v
encodeValue (PersistDay v) = Amazonka.S . T.pack $ show v
encodeValue (PersistTimeOfDay v) = Amazonka.S . T.pack $ show v
encodeValue (PersistUTCTime v) = Amazonka.S . T.pack $ show v
encodeValue PersistNull = Amazonka.NULL
encodeValue (PersistList values) = Amazonka.L . V.fromList . fmap encodeValue $ values
encodeValue (PersistMap entries) = Amazonka.M . Map.fromList . fmap (second encodeValue) $ entries
encodeValue (PersistObjectId v) = encodeValue (PersistByteString v)
encodeValue (PersistArray values) = encodeValue (PersistList values)
encodeValue (PersistLiteral_ _ v) = encodeValue (PersistByteString v)

decodeValue :: Amazonka.AttributeValue -> Either Text PersistValue
decodeValue (Amazonka.L values) = PersistList . V.toList <$> traverse decodeValue values
decodeValue (Amazonka.NS numbers) = decodeValue . Amazonka.L $ Amazonka.S <$> numbers
decodeValue (Amazonka.M m) = PersistMap . Map.toList <$> traverse decodeValue m
decodeValue Amazonka.NULL = Right PersistNull
decodeValue (Amazonka.N (T.unpack -> numStr)) =
  if '.' `elem` numStr
    then bimap T.pack PersistDouble $ readEither numStr
    else bimap T.pack PersistInt64 $ readEither numStr
decodeValue (Amazonka.BS binaries) = decodeValue . Amazonka.L $ Amazonka.B <$> binaries
decodeValue (Amazonka.B binary) = Right . PersistByteString $ Amazonka.unBase64 binary
decodeValue (Amazonka.SS strings) = decodeValue . Amazonka.L $ Amazonka.S <$> strings
decodeValue (Amazonka.S str) = Right $ PersistText str
decodeValue (Amazonka.BOOL bool) = Right $ PersistBool bool

getFieldNames :: forall record. (PersistEntity record) => [Text]
getFieldNames = unFieldNameDB . fieldDB <$> getEntityFields entityDef'
  where
    entityDef' = entityDef (Proxy :: Proxy record)

decodeRecord :: forall record. (PersistEntity record) => HashMap Text Amazonka.AttributeValue -> Either Text record
decodeRecord attributes = do
  fields :: [PersistValue] <- forM (getFieldNames @record) $ \name ->
    maybe (Left $ "field " <> name <> " doesn't exist in the table") decodeValue $
      attributes HashMap.!? name
  fromPersistValues fields

encodeRecord :: forall record. (PersistEntity record) => record -> HashMap Text Amazonka.AttributeValue
encodeRecord record = HashMap.fromList . zip (getFieldNames @record) . fmap encodeValue $ fields
  where
    fields = toPersistFields record

instance PersistStoreRead Backend where
  get ::
    forall m record.
    (MonadIO m, PersistRecordBackend record Backend) =>
    Key record ->
    ReaderT Backend m (Maybe record)
  get key = do
    let entityDef' = entityDef (Proxy :: Proxy record)
        entityIdDef = getEntityId entityDef'
        tableName = escapeWith id $ getEntityDBName entityDef'
    case entityIdDef of
      EntityIdField idFieldDef -> do
        let idFieldName = unFieldNameDB $ fieldDB idFieldDef
            [keyPValue] = keyToValues key
            req = Amazonka.newGetItem tableName & field @"key" . at idFieldName ?~ encodeValue keyPValue
        env <- asks $ view (field @"env")
        resp <- liftIO $ Amazonka.runResourceT $ Amazonka.send env req
        case resp ^. field @"item" of
          Nothing -> pure Nothing
          Just item -> case decodeRecord item of
            Left errMsg -> liftIO . throwIO . DecodeFailure $ errMsg
            Right record -> pure $ Just record
      EntityIdNaturalKey (CompositeDef fields attrs) ->
        -- TODO
        pure Nothing

instance PersistStoreWrite Backend where
  insert record = undefined
  insertKey ::
    forall m record.
    (MonadIO m, PersistRecordBackend record Backend) =>
    Key record ->
    record ->
    ReaderT Backend m ()
  insertKey key record = do
    let entityDef' = entityDef (Proxy :: Proxy record)
        entityIdDef = getEntityId entityDef'
        tableName = escapeWith id $ getEntityDBName entityDef'
    case entityIdDef of
      EntityIdField idFieldDef -> do
        let idFieldName = unFieldNameDB $ fieldDB idFieldDef
            [keyPValue] = keyToValues key
            attributeMap = encodeRecord record <> HashMap.singleton idFieldName (encodeValue keyPValue)
            req = Amazonka.newPutItem tableName & field @"item" .~ attributeMap
        env <- asks $ view (field @"env")
        void . liftIO . Amazonka.runResourceT $ Amazonka.send env req
      EntityIdNaturalKey (CompositeDef fields attrs) -> pure () -- TODO
  repsert = undefined
  replace = undefined
  delete = undefined
  update = undefined
