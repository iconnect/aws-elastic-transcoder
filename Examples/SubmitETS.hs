{-# LANGUAGE OverloadedStrings  #-}


import           Aws
import           Aws.ElasticTranscoder
import           Network.HTTP.Conduit
import qualified Data.Text                  as T
import           Text.Printf
import           System.IO
import           System.Exit
import           System.Environment


preset_id :: PresetId
preset_id = "1351620000001-000010"              -- System preset generic 720p

ets_cfg :: EtsConfiguration NormalQuery
ets_cfg = etsConfiguration HTTPS etsEndpointEu  -- you may have to adjust this
                                                -- to your region


main :: IO ()
main = parse_args >>= create_job


-- | usage: SubmitETS <pipeline-id> <master-s3-uri>

parse_args :: IO (PipelineId,S3Object)
parse_args =
 do as <- getArgs
    case as of
      [pid,s3o] -> return (PipelineId $ T.pack pid,T.pack s3o)
      _         ->
         do pn <- getProgName
            hPutStr stderr $ printf "usage : %s <pipeline-id> <s3-key>\n" pn
            exitWith $ ExitFailure 1

-- | create the job from the key locating the master and streaming outputs
--   within their respective buckets

create_job :: (PipelineId,S3Object) -> IO ()
create_job pid_s30 = 
 do cfg <- Aws.baseConfiguration
    rsp <- withManager $ \mgr -> Aws.pureAws cfg ets_cfg mgr $ mkCJ pid_s30
    print rsp

-- | make up the CreateJob record the key locating the master and streaming outputs
--   within their respective buckets

mkCJ :: (PipelineId,S3Object) -> CreateJob
mkCJ (pid,s3o) = 
    CreateJob
        { cjInput =
            JSInput
                { jsiKey                = s3o
                , jsiFrameRate          = FRauto
                , jsiResolution         = Rauto
                , jsiAspectRatio        = ARauto
                , jsiInterlaced         = ABauto
                , jsiContainer          = Cauto
                }
        , cjOutput =
            JSOutput
                { jsoKey                = s3o
                , jsoThumbnailPattern   = s3o `T.append` "-{count}"
                , jsoRotate             = ROTauto
                , jsoPresetId           = preset_id
                }
        , cjPipelineId = pid
        }
