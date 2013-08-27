{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 

module Aws.ElasticTranscoder.Commands.DeleteJob
    ( DeleteJob(..)
    , DeleteJobResponse(..)
    ) where

import           Aws.Core
import           Aws.ElasticTranscoder.Core
import           Control.Applicative
import qualified Data.Text                      as T
import           Data.Aeson

data DeleteJob
    = DeleteJob
        { djJob :: JobId 
        }
    deriving (Show,Eq)

data DeleteJobResponse
    = DeleteJobResponse 
    deriving (Show,Eq)

instance SignQuery DeleteJob where

    type ServiceConfiguration DeleteJob = EtsConfiguration

    signQuery DeleteJob {..} = etsSignQuery EtsQuery
        { etsqMethod  = Delete
        , etsqRequest = "jobs/" `T.append` _JobId djJob
        , etsqQuery   = []
        , etsqBody    = Nothing
        }

instance ResponseConsumer DeleteJob DeleteJobResponse where

    type ResponseMetadata DeleteJobResponse = EtsMetadata

    responseConsumer _ mref = etsResponseConsumer mref $ \rsp -> 
                                                cnv <$> jsonConsumer rsp
      where
        cnv :: Value -> DeleteJobResponse
        cnv _ = DeleteJobResponse

instance Transaction DeleteJob DeleteJobResponse

instance AsMemoryResponse DeleteJobResponse where

    type MemoryResponse DeleteJobResponse = DeleteJobResponse

    loadToMemory = return
