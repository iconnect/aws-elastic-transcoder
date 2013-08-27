{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 

module Aws.ElasticTranscoder.Commands.CreateJob
    ( CreateJob(..)
    , CreateJobResponse(..)
    , createJob
    , defaultJSInput
    , defaultJSOutput
    ) where

import           Aws.Core
import           Aws.ElasticTranscoder.Core
import           Control.Applicative
import           Data.Aeson


-- | A brief example createJob program
-- 
-- myCreateJob :: IO ()
-- myCreateJob = 
--  do cfg <- Aws.baseConfiguration
--     rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ets_cfg mgr $ 
--                 createJob "Wildlife.wmv" "Wildlife-t.f4v" my_preset my_pipeline
--     print rsp

-- my_ets_cfg :: EtsConfiguration NormalQuery
-- my_ets_cfg = etsConfiguration HTTPS etsEndpointEu

-- my_preset :: PresetId
-- my_preset = "1351620000000-000001"

-- my_pipeline :: PipelineId
-- my_pipeline = "1359460188157-258e48"



data CreateJob
    = CreateJob
        { cjInput      :: JSInput 
        , cjOutput     :: JSOutput
        , cjPipelineId :: PipelineId
        }
    deriving (Show,Eq)

data CreateJobResponse
    = CreateJobResponse
        { cjrId         :: JobId
        , cjrInput      :: JSInput 
        , cjrOutput     :: JSOutputStatus
        , cjrPipelineId :: PipelineId
        }
    deriving (Show,Eq)


createJob :: S3Object -> S3Object -> PresetId -> PipelineId -> CreateJob
createJob inb oub pri pli = CreateJob cji cjo pli
  where
    cji = defaultJSInput  inb
    cjo = defaultJSOutput oub pri


defaultJSInput :: S3Object -> JSInput
defaultJSInput inb = JSInput inb FRauto Rauto ARauto ABauto Cauto

defaultJSOutput :: S3Object -> PresetId -> JSOutput
defaultJSOutput oub pri = JSOutput oub "" ROTauto pri


instance SignQuery CreateJob where

    type ServiceConfiguration CreateJob = EtsConfiguration

    signQuery CreateJob {..} = etsSignQuery EtsQuery
        { etsqMethod  = Post
        , etsqRequest = "jobs"
        , etsqQuery   = []
        , etsqBody    = Just $ toJSON $ JobSpec cjInput cjOutput cjPipelineId
        }

instance ResponseConsumer CreateJob CreateJobResponse where

    type ResponseMetadata CreateJobResponse = EtsMetadata

    responseConsumer _ mref = etsResponseConsumer mref $ \rsp ->
                                                    cnv <$> jsonConsumer rsp
          where
            cnv (JobSingle(JobSpecId a b c d)) = CreateJobResponse a b c d

instance Transaction CreateJob CreateJobResponse

instance AsMemoryResponse CreateJobResponse where

    type MemoryResponse CreateJobResponse = CreateJobResponse

    loadToMemory = return
