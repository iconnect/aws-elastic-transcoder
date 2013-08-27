{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 

module Aws.ElasticTranscoder.Commands.ListJobsByPipeline
    ( ListJobsByPipeline(..)
    , ListJobsByPipelineResponse(..)
    ) where

import           Aws.Core
import           Aws.ElasticTranscoder.Core
import           Control.Applicative
import qualified Data.Text                      as T
import qualified Data.ByteString.Char8          as BC

data ListJobsByPipeline
    = ListJobsByPipeline
        { ljpPipelineId :: PipelineId
        , ljpAscending  :: Bool
        , ljpPageToken  :: TextOrNull
        }
    deriving (Show,Eq)

data ListJobsByPipelineResponse
    = ListJobsByPipelineResponse
        { ljprJobs          :: [JobSpecId]
        , ljprNextPageToken :: TextOrNull 
        }
    deriving (Show,Eq)

instance SignQuery ListJobsByPipeline where

    type ServiceConfiguration ListJobsByPipeline = EtsConfiguration

    signQuery ListJobsByPipeline{..} = etsSignQuery EtsQuery
        { etsqMethod  = Get
        , etsqRequest = "jobsByPipeline/" `T.append` _PipelineId ljpPipelineId
        , etsqQuery   = [("Ascending",text_q $ bool_t ljpAscending)] ++
                        [("pageToken",text_q pgtk) | 
                                            TNText pgtk<-[ljpPageToken] ]
        , etsqBody    = Nothing
        }

instance ResponseConsumer ListJobsByPipeline ListJobsByPipelineResponse where

    type ResponseMetadata ListJobsByPipelineResponse = EtsMetadata

    responseConsumer _ mref = etsResponseConsumer mref $ \rsp ->
                                                    cnv <$> jsonConsumer rsp
          where
            cnv (JobList a b) = ListJobsByPipelineResponse a b

instance Transaction ListJobsByPipeline ListJobsByPipelineResponse

instance AsMemoryResponse ListJobsByPipelineResponse where

    type MemoryResponse ListJobsByPipelineResponse = ListJobsByPipelineResponse

    loadToMemory = return

text_q :: T.Text -> Maybe BC.ByteString
text_q = Just . BC.pack . T.unpack

{-
instance ResponseConsumer ListJobsByPipeline Value where

    type ResponseMetadata Value = EtsMetadata

    responseConsumer _ mref = etsResponseConsumer mref $ \rsp -> jsonConsumer rsp

instance Transaction ListJobsByPipeline Value

instance AsMemoryResponse Value where

    type MemoryResponse Value = Value

    loadToMemory = return
-}
