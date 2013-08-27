{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 

module Aws.ElasticTranscoder.Commands.GetJob
    ( GetJob(..)
    , GetJobResponse(..)
    ) where

import           Aws.Core
import           Aws.ElasticTranscoder.Core
import           Control.Applicative
import qualified Data.Text                      as T


data GetJob
    = GetJob
        { gjJob :: JobId 
        }
    deriving (Show,Eq)

data GetJobResponse
    = GetJobResponse
        { gjrId         :: JobId
        , gjrInput      :: JSInput 
        , gjrOutput     :: JSOutputStatus
        , gjrPipelineId :: PipelineId
        }
    deriving (Show,Eq)

instance SignQuery GetJob where

    type ServiceConfiguration GetJob = EtsConfiguration

    signQuery GetJob {..} = etsSignQuery EtsQuery
        { etsqMethod  = Get
        , etsqRequest = "jobs/" `T.append` _JobId gjJob
        , etsqQuery   = []
        , etsqBody    = Nothing
        }

instance ResponseConsumer GetJob GetJobResponse where

    type ResponseMetadata GetJobResponse = EtsMetadata

    responseConsumer _ mref = etsResponseConsumer mref $ \rsp ->
                                                    cnv <$> jsonConsumer rsp
          where
            cnv (JobSingle(JobSpecId a b c d)) = GetJobResponse a b c d

instance Transaction GetJob GetJobResponse

instance AsMemoryResponse GetJobResponse where

    type MemoryResponse GetJobResponse = GetJobResponse

    loadToMemory = return
