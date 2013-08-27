{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 

module Aws.ElasticTranscoder.Commands.ListJobsByStatus
    ( ListJobsByStatus(..)
    , ListJobsByStatusResponse(..)
    ) where

import           Aws.Core
import           Aws.ElasticTranscoder.Core
import           Control.Applicative
import qualified Data.Text                      as T
import qualified Data.ByteString.Char8          as BC

data ListJobsByStatus
    = ListJobsByStatus
        { ljsStatus    :: Status
        , ljsAscending :: Bool
        , ljsPageToken :: TextOrNull
        }
    deriving (Show,Eq)

data ListJobsByStatusResponse
    = ListJobsByStatusResponse
        { ljsrJobs          :: [JobSpecId]
        , ljsrNextPageToken :: TextOrNull 
        }
    deriving (Show,Eq)

instance SignQuery ListJobsByStatus where

    type ServiceConfiguration ListJobsByStatus = EtsConfiguration

    signQuery ListJobsByStatus{..} = etsSignQuery EtsQuery
        { etsqMethod  = Get
        , etsqRequest = "jobsByStatus/" `T.append` status_t ljsStatus
        , etsqQuery   = [("Ascending",text_q $ bool_t ljsAscending)] ++
                        [("pageToken",text_q pgtk) | 
                                            TNText pgtk<-[ljsPageToken] ]
        , etsqBody    = Nothing
        }

instance ResponseConsumer ListJobsByStatus ListJobsByStatusResponse where

    type ResponseMetadata ListJobsByStatusResponse = EtsMetadata

    responseConsumer _ mref = etsResponseConsumer mref $ \rsp ->
                                                    cnv <$> jsonConsumer rsp
          where
            cnv (JobList a b) = ListJobsByStatusResponse a b

instance Transaction ListJobsByStatus ListJobsByStatusResponse

instance AsMemoryResponse ListJobsByStatusResponse where

    type MemoryResponse ListJobsByStatusResponse = ListJobsByStatusResponse

    loadToMemory = return

text_q :: T.Text -> Maybe BC.ByteString
text_q = Just . BC.pack . T.unpack
