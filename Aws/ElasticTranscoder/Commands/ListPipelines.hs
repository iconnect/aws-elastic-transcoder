{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 

module Aws.ElasticTranscoder.Commands.ListPipelines
    ( ListPipelines(..)
    , ListPipelinesResponse(..)
    ) where

import           Aws.Core
import           Aws.ElasticTranscoder.Core
import           Control.Applicative


data ListPipelines
    = ListPipelines
    deriving (Show,Eq)

newtype ListPipelinesResponse
    = ListPipelinesResponse
        { lprPipelines      :: [PipelineIdStatus]
        }
    deriving (Show,Eq)

instance SignQuery ListPipelines where

    type ServiceConfiguration ListPipelines = EtsConfiguration

    signQuery ListPipelines{..} = etsSignQuery 
        EtsQuery
            { etsqMethod  = Get
            , etsqRequest = "pipelines"
            , etsqQuery   = []
            , etsqBody    = Nothing
            }

instance ResponseConsumer ListPipelines ListPipelinesResponse where

    type ResponseMetadata ListPipelinesResponse = EtsMetadata

    responseConsumer _ mref = etsResponseConsumer mref $ \rsp ->
                                                    cnv <$> jsonConsumer rsp
          where
            cnv (PipelineList pls) = ListPipelinesResponse pls

instance Transaction ListPipelines ListPipelinesResponse

instance AsMemoryResponse ListPipelinesResponse where

    type MemoryResponse ListPipelinesResponse = ListPipelinesResponse

    loadToMemory = return
