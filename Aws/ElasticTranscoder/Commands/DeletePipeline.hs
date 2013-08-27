{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 

module Aws.ElasticTranscoder.Commands.DeletePipeline
    ( DeletePipeline(..)
    , DeletePipelineResponse(..)
    ) where

import           Aws.Core
import           Aws.ElasticTranscoder.Core
import           Control.Applicative
import           Data.Aeson
import           Data.Text                      as T


data DeletePipeline
    = DeletePipeline
        { dpId :: PipelineId
        }
    deriving (Show,Eq)

data DeletePipelineResponse
    = DeletePipelineResponse 
    deriving (Show,Eq)

instance SignQuery DeletePipeline where

    type ServiceConfiguration DeletePipeline = EtsConfiguration

    signQuery DeletePipeline{..} = etsSignQuery 
        EtsQuery
            { etsqMethod  = Delete
            , etsqRequest = "pipelines/" `T.append` _PipelineId dpId
            , etsqQuery   = []
            , etsqBody    = Nothing
            }

instance ResponseConsumer DeletePipeline DeletePipelineResponse where

    type ResponseMetadata DeletePipelineResponse = EtsMetadata

    responseConsumer _ mref = etsResponseConsumer mref $ \rsp ->
                                                    cnv <$> jsonConsumer rsp
          where
            cnv :: Value -> DeletePipelineResponse
            cnv _ = DeletePipelineResponse

instance Transaction DeletePipeline DeletePipelineResponse

instance AsMemoryResponse DeletePipelineResponse where

    type MemoryResponse DeletePipelineResponse = DeletePipelineResponse

    loadToMemory = return
