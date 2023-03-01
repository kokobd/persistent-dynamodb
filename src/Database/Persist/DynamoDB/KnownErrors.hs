{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeApplications #-}

module Database.Persist.DynamoDB.KnownErrors where

import qualified Amazonka
import Control.Lens
import Data.Generics.Product (field)

isConditionalCheckFailed :: Amazonka.Error -> Bool
isConditionalCheckFailed err
  | Amazonka.ServiceError serviceErr <- err,
    serviceErr ^. field @"code" == "ConditionalCheckFailed" =
      True
  | otherwise = False
