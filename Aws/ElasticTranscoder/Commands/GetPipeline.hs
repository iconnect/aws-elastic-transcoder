{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 

module Aws.ElasticTranscoder.Commands.GetPipeline
    ( GetPipeline(..)
    , GetPipelineResponse(..)
    ) where

import           Aws.Core
import           Aws.ElasticTranscoder.Core
import           Control.Applicative
import qualified Data.Text                      as T


data GetPipeline
    = GetPipeline
        { gplPipeline :: PipelineId 
        }
    deriving (Show,Eq)

data GetPipelineResponse
    = GetPipelineResponse
        { gprName          :: PipelineName
        , gprInputBucket   :: S3Object
        , gprOutputBucket  :: S3Object
        , gprRole          :: IAMRole
        , gprNotifications :: Notifications
        , gprId            :: PipelineId
        , gprStatus        :: PipelineStatus
        }
    deriving (Show,Eq)

instance SignQuery GetPipeline where

    type ServiceConfiguration GetPipeline = EtsConfiguration

    signQuery GetPipeline{..} = etsSignQuery 
        EtsQuery
            { etsqMethod  = Get
            , etsqRequest = "pipeline/" `T.append` _PipelineId gplPipeline
            , etsqQuery   = []
            , etsqBody    = Nothing
            }

instance ResponseConsumer GetPipeline GetPipelineResponse where

    type ResponseMetadata GetPipelineResponse = EtsMetadata

    responseConsumer _ mref = etsResponseConsumer mref $ \rsp ->
                                                    cnv <$> jsonConsumer rsp
          where
            cnv (PipelineSingle(PipelineIdStatus a b c d e f g)) = 
                                            GetPipelineResponse a b c d e f g

instance Transaction GetPipeline GetPipelineResponse

instance AsMemoryResponse GetPipelineResponse where

    type MemoryResponse GetPipelineResponse = GetPipelineResponse

    loadToMemory = return
