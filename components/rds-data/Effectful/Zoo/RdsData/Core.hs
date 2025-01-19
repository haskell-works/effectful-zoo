{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Effectful.Zoo.RdsData.Core
  ( executeStatement,
    executeStatement_,
    initialiseDb,
    newExecuteStatement,
    newBatchExecuteStatement,
  ) where

import Amazonka.RDSData.BatchExecuteStatement qualified as AWS
import Amazonka.RDSData.ExecuteStatement qualified as AWS
import Amazonka.Types qualified as AWS
import Data.Generics.Product.Any
import Data.RdsData.Aws
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.Zoo.Amazonka.Api.Send
import Effectful.Zoo.Amazonka.Data
import Effectful.Zoo.Amazonka.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.DataLog.Static
import Effectful.Zoo.Error.Static
import Effectful.Zoo.Log.Api
import Effectful.Zoo.Log.Dynamic
import Effectful.Zoo.RdsData.Errors
import HaskellWorks.Prelude
import Lens.Micro

newExecuteStatement :: ()
  => r <: Reader StatementContext
  => Text
  -> Eff r AWS.ExecuteStatement
newExecuteStatement sql = do
  context <- ask @StatementContext

  let AwsResourceArn theResourceArn = context ^. the @"resourceArn"
  let AwsSecretArn theSecretArn = context ^. the @"secretArn"

  pure $ AWS.newExecuteStatement theResourceArn theSecretArn sql
    & the @"database" .~ (context ^? the @"database" . _Just . the @1)

newBatchExecuteStatement :: ()
  => r <: Reader StatementContext
  => Text
  -> Eff r AWS.BatchExecuteStatement
newBatchExecuteStatement sql = do
  context <- ask @StatementContext

  let AwsResourceArn theResourceArn = context ^. the @"resourceArn"
  let AwsSecretArn theSecretArn = context ^. the @"secretArn"

  pure $ AWS.newBatchExecuteStatement theResourceArn theSecretArn sql
    & the @"database" .~ (context ^? the @"database" . _Just . the @1)

executeStatement :: ()
  => HasCallStack
  => r <: Amazonka
  => r <: DataLog AwsLogEntry
  => r <: Error AWS.Error
  => r <: Error RdsDataError
  => r <: IOE
  => r <: Log Text
  => r <: Reader StatementContext
  => Text
  -> Eff r AWS.ExecuteStatementResponse
executeStatement sql = withFrozenCallStack do
  res <- newExecuteStatement sql >>= sendAws

  case res ^. the @"httpStatus" of
    200 -> do
      info $ "Successfully executed statement.  Results: " <> tshow res
      pure res
    _   -> throw $ RdsDataError $ "Failed to initialise database: " <> tshow res

executeStatement_ :: ()
  => HasCallStack
  => r <: Amazonka
  => r <: DataLog AwsLogEntry
  => r <: Error AWS.Error
  => r <: Error RdsDataError
  => r <: IOE
  => r <: Log Text
  => r <: Reader StatementContext
  => Text
  -> Eff r ()
executeStatement_ f = withFrozenCallStack do
  void $ executeStatement f

initialiseDb :: ()
  => HasCallStack
  => r <: Amazonka
  => r <: DataLog AwsLogEntry
  => r <: Error AWS.Error
  => r <: Error RdsDataError
  => r <: IOE
  => r <: Log Text
  => r <: Reader StatementContext
  => Eff r ()
initialiseDb = withFrozenCallStack do
  executeStatement_ $ mconcat
    [ "CREATE TABLE IF NOT EXISTS migration ("
    , "  ulid CHAR(26)    NOT NULL PRIMARY KEY,"
    , "  created_at       TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,"
    , "  deployed_by      TEXT NOT NULL,"
    , "  CONSTRAINT valid_ulid_constraint"
    , "    CHECK (ulid::text ~ '^[0-9A-HJKMNP-TV-Z]{26}$')"
    , ")"
    ]

  executeStatement_
    "CREATE INDEX IF NOT EXISTS idx_migration_created_at ON migration (created_at)"

  executeStatement_
    "CREATE INDEX IF NOT EXISTS idx_migration_deployed_by ON migration (deployed_by)"
