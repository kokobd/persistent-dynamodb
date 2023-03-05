{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Database.Persist.DynamoDB
  ( -- * Backend
    Backend,
    BackendKey (BackendKeyUUID),
    newBackend,
    defaultMkPersistSettings,

    -- * Error
    Error (..),

    -- * Making raw requests
    tableName,
    sendRequestEither,
    encodeKey,
    encodeRecord,
    decodeRecord,
  )
where

import qualified Amazonka
import qualified Amazonka.DynamoDB as Amazonka
import Control.Exception (Exception, throwIO)
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.Acquire (Acquire)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first, second)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit (ConduitT)
import Data.Foldable (find)
import Data.Function (on)
import Data.Functor (($>))
import Data.Generics.Product (field)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, maybeToList)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (forM)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Vector as V
import Database.Persist
import Database.Persist.DynamoDB.KnownErrors (isConditionalCheckFailed)
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

-- | Use 'Backend' as the backend
defaultMkPersistSettings :: MkPersistSettings
defaultMkPersistSettings = mkPersistSettings (ConT ''Backend)

nextRandomBackendKey :: (MonadIO m) => m (BackendKey Backend)
nextRandomBackendKey = BackendKeyUUID <$> liftIO UUID.nextRandom

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
  | BadEntityImpl Text
  | DuplicateKey
  | InternalError Amazonka.Error
  deriving stock (Show, Generic)
  deriving anyclass (Exception)

throwError :: (MonadIO m) => Error -> m a
throwError = liftIO . throwIO

throwEither :: (MonadIO m) => Either Error a -> m a
throwEither (Left err) = throwError err
throwEither (Right x) = pure x

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

encodeFields :: [(FieldDef, PersistValue)] -> HashMap Text Amazonka.AttributeValue
encodeFields = HashMap.fromList . fmap (bimap (unFieldNameDB . fieldDB) encodeValue)

decodeFields :: [FieldDef] -> HashMap Text Amazonka.AttributeValue -> Either Text [PersistValue]
decodeFields defs attributes = forM fieldNames $ \name ->
  maybe (Left $ "field " <> name <> " doesn't exist in the table") decodeValue $
    attributes HashMap.!? name
  where
    fieldNames = fmap (unFieldNameDB . fieldDB) defs

getRecordFields :: forall record. (PersistEntity record) => [FieldDef]
getRecordFields = getEntityFields entityDef'
  where
    entityDef' = entityDef (Proxy :: Proxy record)

encodeRecord :: forall record. (PersistEntity record) => record -> HashMap Text Amazonka.AttributeValue
encodeRecord record = encodeFields (zip (getRecordFields @record) (toPersistFields record))

decodeRecord :: forall record. (PersistEntity record) => HashMap Text Amazonka.AttributeValue -> Either Error record
decodeRecord attributes = first DecodeFailure $ decodeFields (getRecordFields @record) attributes >>= fromPersistValues

encodeKey :: forall record. (PersistEntity record) => Key record -> HashMap Text Amazonka.AttributeValue
encodeKey key = encodeFields $ zip (entityIdFieldDefs @record) (keyToValues key)

decodeKey :: forall record. (PersistEntity record) => HashMap Text Amazonka.AttributeValue -> Either Error (Key record)
decodeKey attributes = first DecodeFailure $ decodeFields (entityIdFieldDefs @record) attributes >>= keyFromValues

decodeEntity ::
  forall record.
  (PersistEntity record) =>
  HashMap Text Amazonka.AttributeValue ->
  Either Error (Entity record)
decodeEntity attributes = Entity <$> decodeKey attributes <*> decodeRecord attributes

entityIdFieldDefs :: forall record. (PersistEntity record) => [FieldDef]
entityIdFieldDefs = case getEntityId (entityDef (Proxy @record)) of
  EntityIdField idFieldDef -> [idFieldDef]
  EntityIdNaturalKey (CompositeDef fieldDefs' _) -> NonEmpty.toList fieldDefs'

tableName :: forall record. (PersistEntity record) => Text
tableName = escapeWith id $ getEntityDBName (entityDef (Proxy @record))

sendRequest :: forall m req. (MonadIO m, Amazonka.AWSRequest req) => req -> ReaderT Backend m (Amazonka.AWSResponse req)
sendRequest req = do
  env <- asks $ view (field @"env")
  liftIO . Amazonka.runResourceT $ Amazonka.send env req

sendRequestEither ::
  forall m req.
  (MonadIO m, Amazonka.AWSRequest req) =>
  req ->
  ReaderT Backend m (Either Amazonka.Error (Amazonka.AWSResponse req))
sendRequestEither req = do
  env <- asks $ view (field @"env")
  liftIO . Amazonka.runResourceT $ Amazonka.sendEither env req

handleAwsError :: (MonadIO m) => Either Amazonka.Error a -> (Amazonka.Error -> Maybe (m b)) -> (a -> m b) -> m b
handleAwsError (Right a) _ f = f a
handleAwsError (Left err) handleError _ = do
  case handleError err of
    Nothing -> throwError $ InternalError err
    Just m -> m

instance PersistStoreRead Backend where
  get ::
    forall m record.
    (MonadIO m, PersistRecordBackend record Backend) =>
    Key record ->
    ReaderT Backend m (Maybe record)
  get key = do
    resp <- sendRequest $ Amazonka.newGetItem (tableName @record) & field @"key" .~ encodeKey key
    case resp ^. field @"item" of
      Nothing -> pure Nothing
      Just item -> case decodeRecord item of
        Left err -> throwError err
        Right record -> pure $ Just record

instance PersistStoreWrite Backend where
  insert ::
    forall m record.
    (MonadIO m, PersistRecordBackend record Backend, SafeToInsert record) =>
    record ->
    ReaderT Backend m (Key record)
  insert record = do
    key <- case keyFromRecordM of
      Nothing -> do
        backendKey <- nextRandomBackendKey
        either (throwError . BadEntityImpl) pure $
          keyFromValues @record [toPersistValue backendKey]
      Just getKey ->
        pure $ getKey record
    insertKey key record $> key

  insertKey ::
    forall m record.
    (MonadIO m, PersistRecordBackend record Backend) =>
    Key record ->
    record ->
    ReaderT Backend m ()
  insertKey key record =
    case HashMap.toList keyAttrs of
      (attrName, _) : _ -> do
        eitherResp <-
          sendRequestEither $
            Amazonka.newPutItem (tableName @record)
              & field @"item" .~ encodeRecord record <> keyAttrs
              & field @"conditionExpression" ?~ "attribute_not_exists(#c)"
              & field @"expressionAttributeNames" ?~ HashMap.fromList [("#c", attrName)]
        handleAwsError
          eitherResp
          ( \err ->
              if isConditionalCheckFailed err
                then Just (throwError DuplicateKey)
                else Nothing
          )
          (const (pure ()))
      _ -> throwError $ BadEntityImpl "encoded key is empty, check your keyToValues implementation"
    where
      keyAttrs = encodeKey key

  repsert ::
    forall m record.
    (MonadIO m, PersistRecordBackend record Backend) =>
    Key record ->
    record ->
    ReaderT Backend m ()
  repsert key record =
    void . sendRequest $
      Amazonka.newPutItem (tableName @record)
        & field @"item" .~ encodeRecord record <> encodeKey key

  replace ::
    forall m record.
    (MonadIO m, PersistRecordBackend record Backend) =>
    Key record ->
    record ->
    ReaderT Backend m ()
  replace key record =
    case HashMap.toList keyAttrs of
      (attrName, _) : _ -> do
        eitherResp <-
          sendRequestEither $
            Amazonka.newPutItem (tableName @record)
              & field @"item" .~ encodeRecord record <> keyAttrs
              & field @"conditionExpression" ?~ "attribute_exists(#c)"
              & field @"expressionAttributeNames" ?~ HashMap.fromList [("#c", attrName)]
        handleAwsError
          eitherResp
          ( \err ->
              if isConditionalCheckFailed err
                then Just (pure ())
                else Nothing
          )
          (const (pure ()))
      _ -> liftIO . throwIO $ BadEntityImpl "encoded key is empty, check your keyToValues implementation"
    where
      keyAttrs = encodeKey key

  delete ::
    forall m record.
    (MonadIO m, PersistRecordBackend record Backend) =>
    Key record ->
    ReaderT Backend m ()
  delete key =
    void . sendRequest $
      Amazonka.newDeleteItem (tableName @record)
        & field @"key" .~ encodeKey key
  update ::
    forall m record.
    (MonadIO m, PersistRecordBackend record Backend) =>
    Key record ->
    [Update record] ->
    ReaderT Backend m ()
  update _ [] = pure ()
  update key updates =
    void . sendRequest $
      Amazonka.newUpdateItem (tableName @record)
        & field @"key" .~ encodeKey key
        & field @"updateExpression" ?~ updateExpression
        & field @"expressionAttributeNames" ?~ expressionAttributeNames
        & field @"expressionAttributeValues" ?~ expressionAttributeValues
    where
      (updateExpression, expressionAttributeNames, expressionAttributeValues) = renderUpdateExpression updates

renderUpdateExpression ::
  forall record.
  (PersistEntity record) =>
  [Update record] ->
  (Text, HashMap Text Text, HashMap Text Amazonka.AttributeValue)
renderUpdateExpression updates =
  ( "SET " <> T.intercalate ", " (fmap (view _1) rendered),
    HashMap.fromList . Map.toList . Map.unions $ fmap (view _2) rendered,
    HashMap.fromList . Map.toList . Map.unions $ fmap (view _3) rendered
  )
  where
    rendered = catMaybes $ zipWith renderOne [1 ..] updates

    renderOne :: Integer -> Update record -> Maybe (Text, Map Text Text, Map Text Amazonka.AttributeValue)
    renderOne _ (BackendUpdate _) = Nothing
    renderOne idx (Update field' value action) =
      let fieldName = unFieldNameDB . fieldDB . persistFieldDef $ field'
          abstractFieldName = "#c" <> T.pack (show idx)
          abstractValueName = ":v" <> T.pack (show idx)
          expressionAttributeNames = Map.singleton abstractFieldName fieldName
          expressionAttributeValues = Map.singleton abstractValueName $ encodeValue (toPersistValue value)
       in do
            expression <- case action of
              Assign -> Just $ abstractFieldName <> " = " <> abstractValueName
              Add -> Just $ abstractFieldName <> " = " <> abstractFieldName <> " + " <> abstractValueName
              Subtract -> Just $ abstractFieldName <> " = " <> abstractFieldName <> " - " <> abstractValueName
              Multiply -> Nothing -- TODO Add support for multiplication and division
              Divide -> Nothing
              BackendSpecificUpdate _ -> Nothing
            Just (expression, expressionAttributeNames, expressionAttributeValues)

data IndexQueryConditions = IndexQueryConditions
  { iqcIndexName :: Maybe ConstraintNameDB,
    iqcPartitionKey :: (Text, Amazonka.AttributeValue),
    iqcSortKey :: Maybe (Text, Amazonka.AttributeValue)
  }
  deriving (Show, Eq, Generic)

setIndexQueryCondition :: IndexQueryConditions -> Amazonka.Query -> Amazonka.Query
setIndexQueryCondition IndexQueryConditions {..} query =
  query
    & field @"indexName" .~ fmap unConstraintNameDB iqcIndexName
    & field @"keyConditionExpression" ?~ "#pk = :pk, #sk = :sk"
    & field @"expressionAttributeNames"
      ?~ HashMap.fromList
        ( ("#pk", fst iqcPartitionKey)
            : maybeToList (fmap (\(sk, _) -> ("#sk", sk)) iqcSortKey)
        )
    & field @"expressionAttributeValues"
      ?~ HashMap.fromList
        ( (":pk", snd iqcPartitionKey)
            : maybeToList (fmap (\(_, sk) -> (":sk", sk)) iqcSortKey)
        )

encodeUnique ::
  forall record.
  (PersistEntity record) =>
  Unique record ->
  Either Error IndexQueryConditions
encodeUnique unique = do
  uniqueDef <-
    maybe
      (Left $ BadEntityImpl "No UniqueDef for the current unique key is found")
      Right
      uniqueDefMaybe
  let indexName =
        if NonEmpty.toList (uniqueFields uniqueDef) -- primary key doesn't have index name
          == fmap
            (\fieldDef -> (fieldHaskell fieldDef, fieldDB fieldDef))
            (entityIdFieldDefs @record)
          then Nothing
          else Just (uniqueDBName uniqueDef)
  (partitionKey, sortKey) <- pksk
  pure IndexQueryConditions {iqcIndexName = indexName, iqcPartitionKey = partitionKey, iqcSortKey = sortKey}
  where
    uniqueDefMaybe =
      find
        ( \uniqueDef ->
            on
              (==)
              (Set.fromList . NonEmpty.toList)
              (uniqueFields uniqueDef)
              (persistUniqueToFieldNames unique)
        )
        . getEntityUniques
        $ entityDef (Proxy @record)
    pksk =
      case zipWith
        (\(_, unFieldNameDB -> fieldName) value -> (fieldName, encodeValue value))
        (NonEmpty.toList (persistUniqueToFieldNames unique))
        (persistUniqueToValues unique) of
        [partitionKey] -> Right (partitionKey, Nothing)
        [partitionKey, sortKey] -> Right (partitionKey, Just sortKey)
        _ -> Left $ BadEntityImpl "DynamoDB only supports unique index consists of 1 or 2 fields"

instance PersistUniqueRead Backend where
  getBy ::
    forall record m.
    (MonadIO m, PersistRecordBackend record Backend) =>
    Unique record ->
    ReaderT Backend m (Maybe (Entity record))
  getBy unique = do
    indexQueryCondition <- throwEither $ encodeUnique unique
    resp <-
      either (throwError . InternalError) pure
        =<< sendRequestEither
          ( Amazonka.newQuery (tableName @record)
              & setIndexQueryCondition indexQueryCondition
          )
    case resp ^. field @"items" of
      [] -> pure Nothing
      [item] -> fmap Just . either throwError pure $ decodeEntity item
      _ ->
        throwError . BadEntityImpl $
          "More than 1 item returned by Query when calling getBy."
            <> "You probably have a sort key in DDB but not declared in entity"

{-
When an index is found for input conditions, use Query. Otherwise, use Scan
Always try to use Query in PersistUniqueRead
-}

instance PersistQueryRead Backend where
  selectSourceRes ::
    (PersistRecordBackend record Backend, MonadIO m1, MonadIO m2) =>
    [Filter record] ->
    [SelectOpt record] ->
    ReaderT Backend m1 (Acquire (ConduitT () (Entity record) m2 ()))
  selectSourceRes filters opts = undefined

  selectKeysRes ::
    (MonadIO m1, MonadIO m2, PersistRecordBackend record Backend) =>
    [Filter record] ->
    [SelectOpt record] ->
    ReaderT Backend m1 (Acquire (ConduitT () (Key record) m2 ()))
  selectKeysRes = undefined

  count ::
    (MonadIO m, PersistRecordBackend record Backend) =>
    [Filter record] ->
    ReaderT Backend m Int
  count = undefined

  exists ::
    (MonadIO m, PersistRecordBackend record Backend) =>
    [Filter record] ->
    ReaderT Backend m Bool
  exists = undefined
