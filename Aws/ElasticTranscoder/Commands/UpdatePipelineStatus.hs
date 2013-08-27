{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 

module Aws.ElasticTranscoder.Commands.UpdatePipelineStatus
    ( UpdatePipelineStatus(..)
    ) where

import           Aws.Core
import           Aws.ElasticTranscoder.Core
import           Control.Applicative
import           Data.Aeson
import           Data.Text                      as T


data UpdatePipelineStatus
    = UpdatePipelineStatus
        { upsId     :: PipelineId
        , upsStatus :: PipelineStatus
        }
    deriving (Show,Eq)

instance SignQuery UpdatePipelineStatus where

    type ServiceConfiguration UpdatePipelineStatus = EtsConfiguration

    signQuery UpdatePipelineStatus{..} = etsSignQuery 
        EtsQuery
            { etsqMethod  = Get
            , etsqRequest = "pipelines/" `T.append` _PipelineId upsId
                                         `T.append` "/status"
            , etsqQuery   = []
            , etsqBody    = Just $ object [ "Status" .= upsStatus ]
            }

instance ResponseConsumer UpdatePipelineStatus UpdatePipelineStatus where

    type ResponseMetadata UpdatePipelineStatus = EtsMetadata

    responseConsumer _ mref = etsResponseConsumer mref $ \rsp ->
                                                    cnv <$> jsonConsumer rsp
          where
            cnv (PipelineIdAndStatus a b) = UpdatePipelineStatus a b

instance Transaction UpdatePipelineStatus UpdatePipelineStatus

instance AsMemoryResponse UpdatePipelineStatus where

    type MemoryResponse UpdatePipelineStatus = UpdatePipelineStatus

    loadToMemory = return
