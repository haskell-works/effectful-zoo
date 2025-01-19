{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{- HLINT ignore "Use let" -}

module Effectful.Zoo.RdsData.Migration
  ( migrateDown,
    migrateUp,
  ) where

import Amazonka.Types qualified as AWS
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as LBS
import Data.Generics.Product.Any
import Data.List qualified as L
import Data.RdsData.Aws
import Data.RdsData.Migration.Types hiding (id)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.Zoo.Amazonka.Data
import Effectful.Zoo.Amazonka.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.DataLog.Static
import Effectful.Zoo.FileSystem
import Effectful.Zoo.Log.Api
import Effectful.Zoo.Log.Dynamic
import Effectful.Zoo.RdsData.Core
import Effectful.Zoo.RdsData.Errors
import HaskellWorks.Prelude
import Lens.Micro

migrateDown :: ()
  => r <: Amazonka
  => r <: DataLog AwsLogEntry
  => r <: Error AWS.Error
  => r <: Error IOException
  => r <: Error RdsDataError
  => r <: Error YamlDecodeError
  => r <: FileSystem
  => r <: IOE
  => r <: Log Text
  => r <: Reader StatementContext
  => FilePath
  -> Eff r ()
migrateDown migrationFp = do
  value :: Migration <- readYamlFile migrationFp

  let theSteps = value ^.. the @"plan" . to L.reverse . each . the @"steps" . _Just . to L.reverse . each

  forM_ theSteps $ \case
      StepOfDown downStep -> do
        info $ "Executing statement: " <> tshow downStep

        let statement = downStep ^. the @"down" . the @1

        response <- executeStatement statement

        info $ "Results: " <> T.decodeUtf8 (LBS.toStrict (J.encode (response ^. the @"records")))
      StepOfUp _ -> pure ()
      StepOfCreateTable createTableStatement -> do
        statement <- pure $ mconcat
          [ "DROP TABLE " <> createTableStatement ^. the @"createTable" . the @"name"
          ]

        info $ "Executing statement: " <> statement

        response <- executeStatement statement

        info $ "Results: " <> T.decodeUtf8 (LBS.toStrict (J.encode (response ^. the @"records")))
      StepOfCreateIndex createIndexStatement -> do
        statement <- pure $ mconcat
          [ "DROP INDEX " <> createIndexStatement ^. the @"createIndex" . the @"name"
          ]

        info $ "Executing statement: " <> statement

        response <- executeStatement statement

        info $ "Results: " <> T.decodeUtf8 (LBS.toStrict (J.encode (response ^. the @"records")))

migrateUp :: ()
  => r <: Amazonka
  => r <: DataLog AwsLogEntry
  => r <: Error AWS.Error
  => r <: Error IOException
  => r <: Error RdsDataError
  => r <: Error YamlDecodeError
  => r <: FileSystem
  => r <: IOE
  => r <: Log Text
  => r <: Reader StatementContext
  => FilePath
  -> Eff r ()
migrateUp migrationFp = do
  value :: Migration <- readYamlFile migrationFp

  let theSteps = value ^.. the @"plan" . each . the @"steps"  . _Just . each

  forM_ theSteps $ \case
      StepOfUp upStep -> do
        info $ "Executing statement: " <> tshow upStep

        let statement = upStep ^. the @"up" . the @1

        response <- executeStatement statement

        info $ "Results: " <> T.decodeUtf8 (LBS.toStrict (J.encode (response ^. the @"records")))
      StepOfDown _ -> pure ()
      StepOfCreateTable createTableStatement -> do
        columnClauses <- pure $
          createTableStatement ^.. the @"createTable" . the @"columns" . each . to columnToText

        primaryKeyClause <- pure $
          createTableStatement ^.. the @"createTable" . the @"primaryKey" . _Just . to primaryKeyToText

        constraintClauses <- pure $
          createTableStatement ^.. the @"createTable" . the @"constraints" . _Just . each . to constraintToText

        statement <- pure $ mconcat
          [ "CREATE TABLE " <> createTableStatement ^. the @"createTable" . the @"name" <> " ("
          , mconcat $ L.intersperse ", " (columnClauses <> primaryKeyClause <> constraintClauses)
          , ");\n"
          ]

        info $ "Executing create table statement: " <> statement

        response <- executeStatement statement

        info $ "Results: " <> T.decodeUtf8 (LBS.toStrict (J.encode (response ^. the @"records")))
      StepOfCreateIndex createIndexStatement -> do
        columnClauses <- pure $
          createIndexStatement ^.. the @"createIndex" . the @"columns" . each

        statement <- pure $ mconcat
          [ "CREATE INDEX " <> createIndexStatement ^. the @"createIndex" . the @"name"
          , " ON " <> createIndexStatement ^. the @"createIndex" . the @"table" <> " ("
          , mconcat $ L.intersperse ", " columnClauses
          , ");\n"
          ]

        info $ "Executing  create index statement: " <> statement

        response <- executeStatement statement

        info $ "Results: " <> T.decodeUtf8 (LBS.toStrict (J.encode (response ^. the @"records")))

columnToText :: Column -> Text
columnToText c =
  T.intercalate " " $ mconcat
    [ [c ^. the @"name"]
    , [c ^. the @"type_"]
    , [ "NOT NULL"
      | c ^. the @"required"
      ]
    , [ "PRIMARY KEY"
      | c ^. the @"primaryKey"
      ]
    , [ "UNIQUE"
      | c ^. the @"unique"
      ]
    , [ "AUTO_INCREMENT"
      | c ^. the @"autoIncrement"
      ]
    , [ [ "REFERENCES"
        , fk ^. the @"table"
        , "("
        , fk ^. the @"column"
        , ")"
        ] & T.intercalate " "
      | Just fk <- [c ^. the @"references"]
      ]
    ]

primaryKeyToText :: [Text] -> Text
primaryKeyToText cs =
  T.intercalate " "
    [ "PRIMARY KEY"
    , "("
    , T.intercalate ", " cs
    , ")"
    ]

constraintToText :: Constraint -> Text
constraintToText c =
  T.intercalate " "
    [ "CONSTRAINT"
    , c ^. the @"name"
    , "CHECK"
    , "("
    , c ^. the @"check"
    , ")"
    ]
