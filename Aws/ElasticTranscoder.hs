module Aws.ElasticTranscoder
    ( module Aws.ElasticTranscoder.Commands
    , module Aws.ElasticTranscoder.Core
    ) where

import Aws.ElasticTranscoder.Commands
import Aws.ElasticTranscoder.Core

-- $use
--
-- A brief example createJob program
--
-- @
--      {-# LANGUAGE OverloadedStrings  #-}
--      
--      import           Aws
--      import           Aws.ElasticTranscoder
--      import           Network.HTTP.Conduit
--      
--      myCreateJob :: IO ()
--      myCreateJob = 
--       do cfg <- Aws.baseConfiguration
--          rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ets_cfg mgr $ 
--                      createJob "Wildlife.wmv" "Wildlife-t.f4v" my_preset my_pipeline
--          print rsp
--        
--      my_ets_cfg :: EtsConfiguration NormalQuery
--      my_ets_cfg = etsConfiguration HTTPS etsEndpointEu
--              
--      my_preset :: PresetId
--      my_preset = "1351620000001-000001"             -- System preset: Generic 720p
--              
--      my_pipeline :: PipelineId
--      my_pipeline = "<one-of-ypour-pipeline-ids>"
-- @


