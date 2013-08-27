{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 

module Aws.ElasticTranscoder.Commands.CreatePipeline
    ( CreatePipeline(..)
    , CreatePipelineResponse(..)
    ) where

import           Aws.Core
import           Aws.ElasticTranscoder.Core
import           Control.Applicative
import           Data.Aeson


data CreatePipeline
    = CreatePipeline
        { cplName          :: PipelineName
        , cplInputBucket   :: S3Object
        , cplOutputBucket  :: S3Object
        , cplRole          :: IAMRole
        , cplNotifications :: Notifications
        }
    deriving (Show,Eq)

data CreatePipelineResponse
    = CreatePipelineResponse
        { cprName          :: PipelineName
        , cprInputBucket   :: S3Object
        , cprOutputBucket  :: S3Object
        , cprRole          :: IAMRole
        , cprNotifications :: Notifications
        , cprId            :: PipelineId
        , cprStatus        :: PipelineStatus
        }
    deriving (Show,Eq)

instance SignQuery CreatePipeline where

    type ServiceConfiguration CreatePipeline = EtsConfiguration

    signQuery CreatePipeline{..} = etsSignQuery 
        EtsQuery
            { etsqMethod  = Post
            , etsqRequest = "pipeline"
            , etsqQuery   = []
            , etsqBody    = Just $ toJSON $ 
                                Pipeline
                                    cplName 
                                    cplInputBucket
                                    cplOutputBucket
                                    cplRole
                                    cplNotifications
            }

instance ResponseConsumer CreatePipeline CreatePipelineResponse where

    type ResponseMetadata CreatePipelineResponse = EtsMetadata

    responseConsumer _ mref = etsResponseConsumer mref $ \rsp ->
                                                    cnv <$> jsonConsumer rsp
          where
            cnv (PipelineSingle(PipelineIdStatus a b c d e f g)) = 
                                            CreatePipelineResponse a b c d e f g

instance Transaction CreatePipeline CreatePipelineResponse

instance AsMemoryResponse CreatePipelineResponse where

    type MemoryResponse CreatePipelineResponse = CreatePipelineResponse

    loadToMemory = return
