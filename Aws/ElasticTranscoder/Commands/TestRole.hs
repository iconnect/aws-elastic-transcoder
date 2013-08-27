{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 

module Aws.ElasticTranscoder.Commands.TestRole
    ( TestRole(..)
    , TestRoleResponse(..)
    ) where

import           Aws.Core
import           Aws.ElasticTranscoder.Core
import           Control.Applicative
import           Data.Aeson
import           Data.Text                      as T


data TestRole
    = TestRole
        { trInputBucket  :: S3Object
        , trOutputBucket :: S3Object
        , trRole         :: IAMRole
        , trTopics       :: [SNSTopic]
        }
    deriving (Show,Eq)

data TestRoleResponse
    = TestRoleResponse
        { trrMessages :: [T.Text]
        , trrSuccess  :: Bool
        }
    deriving (Show,Eq)

instance SignQuery TestRole where

    type ServiceConfiguration TestRole = EtsConfiguration

    signQuery TestRole{..} = etsSignQuery 
        EtsQuery
            { etsqMethod  = Get
            , etsqRequest = "roleTests"
            , etsqQuery   = []
            , etsqBody    = Just $ toJSON $ 
                                RoleTest
                                    trInputBucket
                                    trOutputBucket
                                    trRole
                                    trTopics
            }

instance ResponseConsumer TestRole TestRoleResponse where

    type ResponseMetadata TestRoleResponse = EtsMetadata

    responseConsumer _ mref = etsResponseConsumer mref $ \rsp ->
                                                    cnv <$> jsonConsumer rsp
          where
            cnv (MessagesSuccess a b) = TestRoleResponse a b

instance Transaction TestRole TestRoleResponse

instance AsMemoryResponse TestRoleResponse where

    type MemoryResponse TestRoleResponse = TestRoleResponse

    loadToMemory = return
