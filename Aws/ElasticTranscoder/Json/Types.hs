{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-} 
{-# LANGUAGE RecordWildCards            #-} 

module Aws.ElasticTranscoder.Json.Types
    ( S3Object
    , SNSTopic
    , emptySNSTopic
    , IAMRole
    , JobId(..)
    , PresetId(..)
    , PageToken(..)
    , JobSpec(..)
    , JobSingle(..)
    , JobList(..)
    , JobSpecId(..)
    , JSInput(..)
    , JSOutput(..)
    , JSOutputStatus(..)
    , FrameRate(..)
    , Resolution(..)
    , AspectRatio(..)
    , Container(..)
    , Rotate(..)
    , Status(..)
    , status_t
    , PipelineName(..)
    , PipelineId(..)
    , Pipeline(..)
    , PipelineSingle(..)
    , PipelineList(..)
    , PipelineIdStatus(..)
    , Notifications(..)
    , PipelineStatus
    , pipelineStatusActive
    , pipelineStatusPaused
    , PipelineIdAndStatus(..)
    , PipelineIdAndNotifications(..)
    , RoleTest(..)
    , PresetName(..)
    , Preset(..)
    , PresetSingle(..)
    , PresetList(..)
    , PresetResponse(..)
    , Audio(..)
    , Video(..)
    , Thumbnails(..)
    , CodecOptions(..)
    , AudioCodec(..)
    , SampleRate(..)
    , Channels(..)
    , VideoCodec(..)
    , ThumbnailFormat(..)
    , Profile(..)
    , Level(..)
    , PresetType(..)
    , BitRate(..)
    , KeyFrameRate(..)
    , FixedGOP(..)
    , Interval(..)
    , MessagesSuccess(..)
    , AutoBool(..)
    , TextOrNull(..)
    , SUCCESS(..)
    , EtsServiceError(..)
    , bool_t
    ) where

import           Control.Monad
import           Control.Applicative
import           Text.Printf
import           Text.Regex
import           Data.String
import qualified Data.Map                       as Map
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text                      as T
import qualified Test.QuickCheck                as QC
import           Safe


--
-- | Aws.S3 uses Text for object keys
--

type S3Object = T.Text


--
-- | SNS Topics represented by Text values
--

type SNSTopic = T.Text

emptySNSTopic :: SNSTopic
emptySNSTopic = ""


--
-- | IAM Role represented by Yext
--

type IAMRole = T.Text


--
-- | Job Identifiers
--

newtype JobId = JobId { _JobId :: T.Text }
    deriving (Show,IsString,Eq)

instance FromJSON JobId where
    parseJSON = withText "JobId" $ return . JobId

instance ToJSON JobId where
    toJSON = String . _JobId

instance QC.Arbitrary JobId where
    arbitrary = JobId . T.pack <$> QC.arbitrary


--
-- | Preset Identifiers
--

newtype PresetId = PresetId { _PresetId :: T.Text }
    deriving (Show,IsString,Eq)

instance FromJSON PresetId where
    parseJSON = withText "PresetId" $ return . PresetId

instance ToJSON PresetId where
    toJSON = String . _PresetId

instance QC.Arbitrary PresetId where
    arbitrary = PresetId . T.pack <$> QC.arbitrary



--
-- | Page Tokens
--

newtype PageToken = PageToken { _PageToken :: T.Text }
    deriving (Show,IsString,Eq)

instance FromJSON PageToken where
    parseJSON = withText "PageToken" $ return . PageToken

instance ToJSON PageToken where
    toJSON = String . _PageToken

instance QC.Arbitrary PageToken where
    arbitrary = PageToken . T.pack <$> QC.arbitrary



--
-- | Job Specifications
--

data JobSpec
    = JobSpec
        { jsInput      :: JSInput 
        , jsOutput     :: JSOutput
        , jsPipelineId :: PipelineId
        }
    deriving (Show,Eq)

instance FromJSON JobSpec where
     parseJSON (Object v) = 
        JobSpec <$>
            v .: "Input"                            <*>
            v .: "Output"                           <*>
            v .: "PipelineId"
     parseJSON _          = mzero

instance ToJSON JobSpec where
     toJSON js@(JobSpec _ _ _) =
        object 
            [ "Input"      .= jsInput      js
            , "Output"     .= jsOutput     js
            , "PipelineId" .= jsPipelineId js
            ]

instance QC.Arbitrary JobSpec where
    arbitrary = JobSpec <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary


--
-- | Job Single
--

newtype JobSingle
    = JobSingle
        { jsJob :: JobSpecId
        }
    deriving (Show,Eq)

instance FromJSON JobSingle where
     parseJSON (Object v) = 
        JobSingle <$>
            v .: "Job"
     parseJSON _          = mzero

instance ToJSON JobSingle where
     toJSON js =
        object 
            [ "Job" .= jsJob js
            ]

instance QC.Arbitrary JobSingle where
    arbitrary = JobSingle <$> QC.arbitrary


--
-- | Job List
--

data JobList
    = JobList
        { jlJobs          :: [JobSpecId]
        , jlNextPageToken :: TextOrNull
        }
    deriving (Show,Eq)

instance FromJSON JobList where
     parseJSON (Object v) = 
        JobList <$>
            v .: "Jobs"                             <*>
            v .: "NextPageToken"
     parseJSON _          = mzero

instance ToJSON JobList where
     toJSON js@(JobList _ _) =
        object 
            [ "Jobs"          .= jlJobs          js
            , "NextPageToken" .= jlNextPageToken js
            ]

instance QC.Arbitrary JobList where
    arbitrary = JobList <$> QC.arbitrary <*> QC.arbitrary


--
-- | Job Specifications with JobId & Status
--

data JobSpecId
    = JobSpecId
        { jsiId         :: JobId
        , jsiInput      :: JSInput 
        , jsiOutput     :: JSOutputStatus
        , jsiPipelineId :: PipelineId
        }
    deriving (Show,Eq)

instance FromJSON JobSpecId where
     parseJSON (Object v) = 
        JobSpecId <$>
            v .: "Id"                               <*>
            v .: "Input"                            <*>
            v .: "Output"                           <*>
            v .: "PipelineId"
     parseJSON _          = mzero

instance ToJSON JobSpecId where
     toJSON jsi@(JobSpecId _ _ _ _) =
        object
            [ "Id"         .= jsiId         jsi
            , "Input"      .= jsiInput      jsi
            , "Output"     .= jsiOutput     jsi
            , "PipelineId" .= jsiPipelineId jsi
            ]

instance QC.Arbitrary JobSpecId where
    arbitrary = 
        JobSpecId 
            <$> QC.arbitrary 
            <*> QC.arbitrary 
            <*> QC.arbitrary
            <*> QC.arbitrary


--
-- | Job Input Parameters
--

data JSInput 
    = JSInput
        { jsiKey                :: S3Object
        , jsiFrameRate          :: FrameRate  
        , jsiResolution         :: Resolution
        , jsiAspectRatio        :: AspectRatio
        , jsiInterlaced         :: AutoBool
        , jsiContainer          :: Container
        }
    deriving (Show,Eq)

instance FromJSON JSInput where
    parseJSON (Object v) = 
        JSInput <$>
            v .: "Key"                          <*>
            v .: "FrameRate"                    <*>
            v .: "Resolution"                   <*>
            v .: "AspectRatio"                  <*>
            v .: "Interlaced"                   <*>
            v .: "Container"
    parseJSON _          = mzero

instance ToJSON JSInput where
    toJSON ijs@(JSInput _ _ _ _ _ _) =
        object 
            [ "Key"         .= jsiKey         ijs
            , "FrameRate"   .= jsiFrameRate   ijs
            , "Resolution"  .= jsiResolution  ijs
            , "AspectRatio" .= jsiAspectRatio ijs
            , "Interlaced"  .= jsiInterlaced  ijs
            , "Container"   .= jsiContainer   ijs
            ]

instance QC.Arbitrary JSInput where
    arbitrary =
        JSInput 
            <$> (T.pack <$> QC.arbitrary) 
            <*> QC.arbitrary 
            <*> QC.arbitrary
            <*> QC.arbitrary
            <*> QC.arbitrary
            <*> QC.arbitrary


--
-- | Job Output Parameters
--

data JSOutput 
    = JSOutput
        { jsoKey              :: S3Object
        , jsoThumbnailPattern :: T.Text
        , jsoRotate           :: Rotate
        , jsoPresetId         :: PresetId
        }
    deriving (Show,Eq)

instance FromJSON JSOutput where
    parseJSON (Object v) = 
        JSOutput <$>
            v .: "Key"                              <*>
            v .: "ThumbnailPattern"                 <*>
            v .: "Rotate"                           <*>
            v .: "PresetId"
    parseJSON _          = mzero

instance ToJSON JSOutput where
    toJSON jso@(JSOutput _ _ _ _) =
        object 
            [ "Key"             .= jsoKey              jso
            , "ThumbnailPattern".= jsoThumbnailPattern jso
            , "Rotate"          .= jsoRotate           jso
            , "PresetId"        .= jsoPresetId         jso
            ]

instance QC.Arbitrary JSOutput where
    arbitrary = JSOutput 
                    <$> (T.pack <$> QC.arbitrary) 
                    <*> (T.pack <$> QC.arbitrary)
                    <*>             QC.arbitrary
                    <*>             QC.arbitrary


--
-- | Job Output Parameters with Status
--

data JSOutputStatus 
    = JSOutputStatus
        { jsosKey              :: S3Object
        , jsosThumbnailPattern :: Maybe T.Text
        , jsosRotate           :: Rotate
        , jsosPresetId         :: PresetId
        , jsosStatus           :: Status
        , jsosStatusDetail     :: TextOrNull
        }
    deriving (Show,Eq)

instance FromJSON JSOutputStatus where
    parseJSON (Object v) = 
        JSOutputStatus <$>
            v .: "Key"                              <*>
            v .: "ThumbnailPattern"                 <*>
            v .: "Rotate"                           <*>
            v .: "PresetId"                         <*>
            v .: "Status"                           <*>
            v .: "StatusDetail"
    parseJSON _          = mzero

instance ToJSON JSOutputStatus where
    toJSON jsos@(JSOutputStatus _ _ _ _ _ _) =
        object 
            [ "Key"             .= jsosKey               jsos
            , "ThumbnailPattern".= jsosThumbnailPattern  jsos
            , "Rotate"          .= jsosRotate            jsos
            , "PresetId"        .= jsosPresetId          jsos
            , "Status"          .= jsosStatus            jsos
            , "StatusDetail"    .= jsosStatusDetail      jsos
            ]

instance QC.Arbitrary JSOutputStatus where
    arbitrary = JSOutputStatus 
                    <$> (     T.pack <$> QC.arbitrary) 
                    <*> (fmap T.pack <$> QC.arbitrary)
                    <*>                  QC.arbitrary
                    <*>                  QC.arbitrary
                    <*>                  QC.arbitrary
                    <*>                  QC.arbitrary


--
-- | Input Frame Rate
--

data FrameRate
    = FRauto
    | FR10
    | FR15
    | FR23_97
    | FR24
    | FR25
    | FR29_97
    | FR30
    | FR60
    deriving (Show,Eq,Ord,Bounded,Enum)

framerate_t :: FrameRate -> T.Text
framerate_t fr =
        case fr of
          FRauto    -> "auto"
          FR10      -> "10" 
          FR15      -> "15"
          FR23_97   -> "23.97"
          FR24      -> "24"
          FR25      -> "25"
          FR29_97   -> "29.97"
          FR30      -> "30"
          FR60      -> "60"

framerate_m :: Map.Map T.Text FrameRate
framerate_m = text_map framerate_t
    
instance FromJSON FrameRate where
    parseJSON = json_str_map_p framerate_m

instance ToJSON FrameRate where
    toJSON = String . framerate_t

instance QC.Arbitrary FrameRate where
    arbitrary = QC.elements [minBound..maxBound]


--
-- | Input Resolution
--

data Resolution
    = Rauto
    | Rpixels (Int,Int)
    deriving (Show,Eq)

resolution_t :: Resolution -> T.Text
resolution_t fr =
    case fr of
      Rauto         -> "auto"
      Rpixels (w,h) -> T.pack $ printf "%dx%d" w h

instance FromJSON Resolution where
    parseJSON = withText "Resolution" $ parse_res . T.unpack

instance ToJSON Resolution where
    toJSON = String . resolution_t

instance QC.Arbitrary Resolution where
    arbitrary = inj <$> poss nat_pair
          where
            inj Nothing  = Rauto
            inj (Just p) = Rpixels p

parse_res :: String -> Parser Resolution
parse_res "auto" = return Rauto
parse_res s      = maybe err return $
     do [ws,hs] <- matchRegex res_re s
        w       <- readMay ws 
        h       <- readMay hs
        return $ Rpixels (w,h)
      where
        err = typeMismatch "resolution" $ toJSON s

res_re :: Regex
res_re = mkRegex "([0-9]+)[xX]([0-9]+)"


--
-- | Input Aspect Ratio
--

data AspectRatio
    = ARauto
    | AR1_1
    | AR4_3
    | AR3_2
    | AR16_9
    deriving (Show,Eq,Ord,Bounded,Enum)

aspectratio_t :: AspectRatio -> T.Text
aspectratio_t fr =
        case fr of
          ARauto    -> "auto"
          AR1_1     -> "1:1"
          AR4_3     -> "4:3"
          AR3_2     -> "3:2"
          AR16_9    -> "16:9"

aspectratio_m :: Map.Map T.Text AspectRatio
aspectratio_m = text_map aspectratio_t
    
instance FromJSON AspectRatio where
    parseJSON = json_str_map_p aspectratio_m

instance ToJSON AspectRatio where
    toJSON = String . aspectratio_t

instance QC.Arbitrary AspectRatio where
    arbitrary = QC.elements [minBound..maxBound]



--
-- | Input Container Type
--

data Container
    = Cauto
    | C3gp
    | Casf
    | Cavi
    | Cdivx
    | Cflv
    | Cmkv
    | Cmov
    | Cmp4
    | Cmpeg
    | Cmpeg_ps
    | Cmpeg_ts
    | Cmxf
    | Cogg
    | Cvob
    | Cwav
    | Cwebm
    deriving (Show,Eq,Ord,Bounded,Enum)

container_t :: Container -> T.Text
container_t fr =
    case fr of
      Cauto     -> "auto"
      C3gp      -> "3gp"
      Casf      -> "asf"
      Cavi      -> "avi"
      Cdivx     -> "divx"
      Cflv      -> "flv"
      Cmkv      -> "mkv"
      Cmov      -> "mov"
      Cmp4      -> "mp4"
      Cmpeg     -> "mpeg"
      Cmpeg_ps  -> "mpeg-ps"
      Cmpeg_ts  -> "mpeg-ts"
      Cmxf      -> "mxf"
      Cogg      -> "ogg"
      Cvob      -> "vob"
      Cwav      -> "wav"
      Cwebm     -> "webm"

container_m :: Map.Map T.Text Container
container_m = text_map container_t
    
instance FromJSON Container where
    parseJSON = json_str_map_p container_m

instance ToJSON Container where
    toJSON = String . container_t

instance QC.Arbitrary Container where
    arbitrary = QC.elements [minBound..maxBound]


--
-- | Output Rotation
--

data Rotate
    = ROTauto
    | ROT0
    | ROT90
    | ROT180
    | ROT270
    deriving (Show,Eq,Ord,Bounded,Enum)

rotate_t :: Rotate -> T.Text
rotate_t rot =
        case rot of
          ROTauto   -> "auto"
          ROT0      -> "0"
          ROT90     -> "90"
          ROT180    -> "180"
          ROT270    -> "270"

rotate_m :: Map.Map T.Text Rotate
rotate_m = text_map rotate_t

instance FromJSON Rotate where
    parseJSON = json_str_map_p rotate_m

instance ToJSON Rotate where
    toJSON = String . rotate_t

instance QC.Arbitrary Rotate where
    arbitrary = QC.elements [minBound..maxBound]


--
-- | Job Status
--

data Status
    = STSSubmitted
    | STSProgressing
    | STSComplete
    | STSCancelled
    | STSError
    deriving (Show,Eq,Ord,Bounded,Enum)

status_t :: Status -> T.Text
status_t sts =
    case sts of
      STSSubmitted   -> "Submitted"
      STSProgressing -> "Progressing"
      STSComplete    -> "Complete"
      STSCancelled   -> "Canceled"
      STSError       -> "Error"

status_m :: Map.Map T.Text Status
status_m = text_map status_t

instance FromJSON Status where
    parseJSON = json_str_map_p status_m

instance ToJSON Status where
    toJSON = String . status_t

instance QC.Arbitrary Status where
    arbitrary = QC.elements [minBound..maxBound]


--
-- | PipelineName
--

newtype PipelineName = PipelineName { _PipelineName :: T.Text }
    deriving (Show,IsString,Eq)

instance FromJSON PipelineName where
    parseJSON = withText "PipelineName" $ return . PipelineName

instance ToJSON PipelineName where
    toJSON = String . _PipelineName

instance QC.Arbitrary PipelineName where
    arbitrary = PipelineName . T.pack <$> QC.arbitrary


--
-- | PipelineId
--

newtype PipelineId = PipelineId { _PipelineId :: T.Text }
    deriving (Show,IsString,Eq)

instance FromJSON PipelineId where
    parseJSON = withText "PipelineId" $ return . PipelineId

instance ToJSON PipelineId where
    toJSON = String . _PipelineId

instance QC.Arbitrary PipelineId where
    arbitrary = PipelineId . T.pack <$> QC.arbitrary


--
-- | Pipeline
--

data Pipeline
    = Pipeline
        { plnName          :: PipelineName
        , plnInputBucket   :: S3Object
        , plnOutputBucket  :: S3Object
        , plnRole          :: IAMRole
        , plnNotifications :: Notifications
        }
    deriving (Show,Eq)

instance FromJSON Pipeline where
    parseJSON (Object v) = 
        Pipeline <$>
            v .: "Name"                             <*>
            v .: "InputBucket"                      <*>
            v .: "OutputBucket"                     <*>
            v .: "Role"                             <*>
            v .: "Notifications"
    parseJSON _          = mzero

instance ToJSON Pipeline where
    toJSON pln@(Pipeline _ _ _ _ _) =
        object 
            [ "Name"          .= plnName          pln
            , "InputBucket"   .= plnInputBucket   pln
            , "OutputBucket"  .= plnOutputBucket  pln
            , "Role"          .= plnRole          pln
            , "Notifications" .= plnNotifications pln
            ]

instance QC.Arbitrary Pipeline where
    arbitrary = Pipeline 
                    <$>             QC.arbitrary 
                    <*> (T.pack <$> QC.arbitrary)
                    <*> (T.pack <$> QC.arbitrary)
                    <*> (T.pack <$> QC.arbitrary)
                    <*>             QC.arbitrary


--
-- | Pipeline Single
--

newtype PipelineSingle
    = PipelineSingle
        { psPipeline :: PipelineIdStatus
        }
    deriving (Show,Eq)

instance FromJSON PipelineSingle where
     parseJSON (Object v) = 
        PipelineSingle <$>
            v .: "Pipeline"
     parseJSON _          = mzero

instance ToJSON PipelineSingle where
     toJSON js =
        object 
            [ "Pipeline" .= psPipeline js
            ]

instance QC.Arbitrary PipelineSingle where
    arbitrary = PipelineSingle <$> QC.arbitrary


--
-- | Pipeline List
--

data PipelineList
    = PipelineList
        { plPipelines     :: [PipelineIdStatus]
        }
    deriving (Show,Eq)

instance FromJSON PipelineList where
     parseJSON (Object v) = 
        PipelineList <$>
            v .: "Pipelines"
     parseJSON _          = mzero

instance ToJSON PipelineList where
     toJSON ps@(PipelineList _) =
        object 
            [ "Pipelines"     .= plPipelines     ps
            ]

instance QC.Arbitrary PipelineList where
    arbitrary = PipelineList <$> QC.arbitrary


--
-- | PipelineIdStatus
--

data PipelineIdStatus
    = PipelineIdStatus
        { pisName          :: PipelineName
        , pisInputBucket   :: S3Object
        , pisOutputBucket  :: S3Object
        , pisRole          :: IAMRole
        , pisNotifications :: Notifications
        , pisId            :: PipelineId
        , pisStatus        :: PipelineStatus
        }
    deriving (Show,Eq)

instance FromJSON PipelineIdStatus where
    parseJSON (Object v) = 
        PipelineIdStatus <$>
            v .: "Name"                             <*>
            v .: "InputBucket"                      <*>
            v .: "OutputBucket"                     <*>
            v .: "Role"                             <*>
            v .: "Notifications"                    <*>
            v .: "Id"                               <*>
            v .: "Status"
    parseJSON _          = mzero

instance ToJSON PipelineIdStatus where
    toJSON pis@(PipelineIdStatus _ _ _ _ _ _ _) =
        object 
            [ "Name"          .= pisName          pis
            , "InputBucket"   .= pisInputBucket   pis
            , "OutputBucket"  .= pisOutputBucket  pis
            , "Role"          .= pisRole          pis
            , "Notifications" .= pisNotifications pis
            , "Id"            .= pisId            pis
            , "Status"        .= pisStatus        pis
            ]

instance QC.Arbitrary PipelineIdStatus where
    arbitrary = PipelineIdStatus 
                    <$>             QC.arbitrary 
                    <*> (T.pack <$> QC.arbitrary)
                    <*> (T.pack <$> QC.arbitrary)
                    <*> (T.pack <$> QC.arbitrary)
                    <*>             QC.arbitrary
                    <*>             QC.arbitrary
                    <*> (T.pack <$> QC.arbitrary)


--
-- | Notifications
--

data Notifications
    = Notifications
        { ntfCompleted    :: SNSTopic
        , ntfError        :: SNSTopic
        , ntfProgressing  :: SNSTopic
        , ntfWarning      :: SNSTopic
        }
    deriving (Show,Eq)

instance FromJSON Notifications where
    parseJSON (Object v) = 
        Notifications <$>
            v .: "Completed"                        <*>
            v .: "Error"                            <*>
            v .: "Progressing"                      <*>
            v .: "Warning"
    parseJSON _          = mzero

instance ToJSON Notifications where
    toJSON ntf@(Notifications _ _ _ _) =
        object 
            [ "Completed"       .= ntfCompleted        ntf
            , "Error"           .= ntfError            ntf
            , "Progressing"     .= ntfProgressing      ntf
            , "Warning"         .= ntfWarning          ntf
            ]

instance QC.Arbitrary Notifications where
    arbitrary = Notifications 
                    <$> (T.pack <$> QC.arbitrary) 
                    <*> (T.pack <$> QC.arbitrary)
                    <*> (T.pack <$> QC.arbitrary)
                    <*> (T.pack <$> QC.arbitrary)


--
-- | PipelineStatus
--

-- Documentation is disturbingly vague on the values this type can
-- take so we represent it with Text

type PipelineStatus = T.Text

pipelineStatusActive :: PipelineStatus
pipelineStatusActive = "active"

pipelineStatusPaused :: PipelineStatus
pipelineStatusPaused = "paused"


--
-- | PipelineIdAndStatus
--

data PipelineIdAndStatus
    = PipelineIdAndStatus
        { pasId     :: PipelineId
        , pasStatus :: PipelineStatus
        }
    deriving (Show,Eq)

instance FromJSON PipelineIdAndStatus where
    parseJSON (Object v) = 
        PipelineIdAndStatus <$>
            v .: "Id"                               <*>
            v .: "Status"
    parseJSON _          = mzero

instance ToJSON PipelineIdAndStatus where
    toJSON pas@(PipelineIdAndStatus _ _) =
        object 
            [ "Id"              .= pasId     pas
            , "Status"          .= pasStatus pas
            ]

instance QC.Arbitrary PipelineIdAndStatus where
    arbitrary = PipelineIdAndStatus 
                    <$>             QC.arbitrary 
                    <*> (T.pack <$> QC.arbitrary)


--
-- | PipelineIdAndNotifications
--

data PipelineIdAndNotifications
    = PipelineIdAndNotifications
        { panId            :: PipelineId
        , panNotifications :: Notifications
        }
    deriving (Show,Eq)

instance FromJSON PipelineIdAndNotifications where
    parseJSON (Object v) = 
        PipelineIdAndNotifications <$>
            v .: "Id"                               <*>
            v .: "Notifications"
    parseJSON _          = mzero

instance ToJSON PipelineIdAndNotifications where
    toJSON pas@(PipelineIdAndNotifications _ _) =
        object 
            [ "Id"              .= panId            pas
            , "Notifications"   .= panNotifications pas
            ]

instance QC.Arbitrary PipelineIdAndNotifications where
    arbitrary = PipelineIdAndNotifications 
                    <$> QC.arbitrary 
                    <*> QC.arbitrary


--
-- | RoleTest
--

data RoleTest
    = RoleTest
        { rtInputBucket  :: S3Object
        , rtOutputBucket :: S3Object
        , rtRole         :: IAMRole
        , rtTopics       :: [SNSTopic]
        }
    deriving (Show,Eq)

instance FromJSON RoleTest where
    parseJSON (Object v) = 
        RoleTest <$>
            v .: "InputBucket"                      <*>
            v .: "OutputBucket"                     <*>
            v .: "Role"                             <*>
            v .: "Topics"
    parseJSON _          = mzero

instance ToJSON RoleTest where
    toJSON rt@(RoleTest _ _ _ _) =
        object 
            [ "InputBucket"  .= rtInputBucket  rt
            , "OutputBucket" .= rtOutputBucket rt
            , "Role"         .= rtRole         rt
            , "Topics"       .= rtTopics       rt
            ]

instance QC.Arbitrary RoleTest where
    arbitrary = RoleTest
                    <$> (    T.pack <$> QC.arbitrary) 
                    <*> (    T.pack <$> QC.arbitrary)
                    <*> (    T.pack <$> QC.arbitrary)
                    <*> (map T.pack <$> QC.arbitrary)




--
-- | PresetName
--

newtype PresetName = PresetName { _PresetName :: T.Text }
    deriving (Show,IsString,Eq)

instance FromJSON PresetName where
    parseJSON = withText "PresetName" $ return . PresetName

instance ToJSON PresetName where
    toJSON    = String . _PresetName

instance QC.Arbitrary PresetName where
    arbitrary = PresetName . T.pack <$> QC.arbitrary


--
-- | Preset
--

data Preset
    = Preset
        { prName        :: PresetName
        , prDescription :: T.Text
        , prContainer   :: Container
        , prAudio       :: Audio
        , prVideo       :: Video
        , prThumbnails  :: Thumbnails
        }
    deriving (Show,Eq)

instance FromJSON Preset where
    parseJSON (Object v) = 
        Preset <$>
            v .: "Name"                             <*>
            v .: "Description"                      <*>
            v .: "Container"                        <*>
            v .: "Audio"                            <*>
            v .: "Video"                            <*>
            v .: "Thumbnails"
    parseJSON _          = mzero

instance ToJSON Preset where
    toJSON pr@(Preset _ _ _ _ _ _) =
        object 
            [ "Name"        .= prName        pr
            , "Description" .= prDescription pr
            , "Container"   .= prContainer   pr
            , "Audio"       .= prAudio       pr
            , "Video"       .= prVideo       pr
            , "Thumbnails"  .= prThumbnails  pr
            ]

instance QC.Arbitrary Preset where
    arbitrary = Preset 
                    <$>             QC.arbitrary 
                    <*> (T.pack <$> QC.arbitrary)
                    <*>             QC.arbitrary
                    <*>             QC.arbitrary
                    <*>             QC.arbitrary
                    <*>             QC.arbitrary


--
-- | PresetSingle
--

newtype PresetSingle
    = PresetSingle
        { psPreset :: PresetResponse
        }
    deriving (Show,Eq)

instance FromJSON PresetSingle where
     parseJSON (Object v) = 
        PresetSingle <$>
            v .: "Preset"
     parseJSON _          = mzero

instance ToJSON PresetSingle where
     toJSON js =
        object 
            [ "Preset" .= psPreset js
            ]

instance QC.Arbitrary PresetSingle where
    arbitrary = PresetSingle <$> QC.arbitrary


--
-- | PresetList
--

data PresetList
    = PresetList
        { plPresets     :: [PresetResponse]
        }
    deriving (Show,Eq)

instance FromJSON PresetList where
     parseJSON (Object v) = 
        PresetList <$>
            v .: "Presets"
     parseJSON _          = mzero

instance ToJSON PresetList where
     toJSON ps@(PresetList _) =
        object 
            [ "Presets"     .= plPresets     ps
            ]

instance QC.Arbitrary PresetList where
    arbitrary = PresetList <$> QC.arbitrary


--
-- | PresetResponse
--

data PresetResponse
    = PresetResponse
        { prrName        :: PresetName
        , prrDescription :: T.Text
        , prrContainer   :: Container
        , prrAudio       :: Audio
        , prrVideo       :: Video
        , prrThumbnails  :: Thumbnails
        , prrId          :: PresetId
        , prrType        :: PresetType
        , prrWarning     :: T.Text
        }
    deriving (Show,Eq)

instance FromJSON PresetResponse where
    parseJSON (Object v) = 
        PresetResponse <$>
            v .: "Name"                             <*>
            v .: "Description"                      <*>
            v .: "Container"                        <*>
            v .: "Audio"                            <*>
            v .: "Video"                            <*>
            v .: "Thumbnails"                       <*>
            v .: "Id"                               <*>
            v .: "Type"                             <*>
            v .: "Warning"
    parseJSON _          = mzero

instance ToJSON PresetResponse where
    toJSON prr@(PresetResponse _ _ _ _ _ _ _ _ _) =
        object 
            [ "Name"        .= prrName        prr
            , "Description" .= prrDescription prr
            , "Container"   .= prrContainer   prr
            , "Audio"       .= prrAudio       prr
            , "Video"       .= prrVideo       prr
            , "Thumbnails"  .= prrThumbnails  prr
            , "Id"          .= prrId          prr
            , "Type"        .= prrType        prr
            , "Warning"     .= prrWarning     prr
            ]

instance QC.Arbitrary PresetResponse where
    arbitrary = PresetResponse 
                    <$>             QC.arbitrary 
                    <*> (T.pack <$> QC.arbitrary)
                    <*>             QC.arbitrary
                    <*>             QC.arbitrary
                    <*>             QC.arbitrary
                    <*>             QC.arbitrary
                    <*>             QC.arbitrary
                    <*>             QC.arbitrary
                    <*> (T.pack <$> QC.arbitrary)


--
-- | Audio
--

data Audio
    = Audio
        { auCodec      :: AudioCodec
        , auSampleRate :: SampleRate
        , auBitRate    :: BitRate
        , auChannels   :: Channels
        }
    deriving (Show,Eq)

instance FromJSON Audio where
    parseJSON (Object v) = 
        Audio <$>
            v .: "Codec"                            <*>
            v .: "SampleRate"                       <*>
            v .: "BitRate"                          <*>
            v .: "Channels"
    parseJSON _          = mzero

instance ToJSON Audio where
    toJSON au@(Audio _ _ _ _) =
        object 
            [ "Codec"      .= auCodec      au
            , "SampleRate" .= auSampleRate au
            , "BitRate"    .= auBitRate    au
            , "Channels"   .= auChannels   au
            ]

instance QC.Arbitrary Audio where
    arbitrary = Audio
                    <$> QC.arbitrary 
                    <*> QC.arbitrary
                    <*> QC.arbitrary
                    <*> QC.arbitrary


--
-- | Video
--

data Video
    = Video
        { vdCodec            :: VideoCodec 
        , vdCodecOptions     :: CodecOptions
        , vdKeyFrameRateDist :: KeyFrameRate
        , vdFixedGOP         :: FixedGOP
        , vdBitRate          :: BitRate
        , vdFrameRate        :: FrameRate
        , vdResolution       :: Resolution 
        , vdAspectRatio      :: AspectRatio
        }
    deriving (Show,Eq)

instance FromJSON Video where
    parseJSON (Object v) = 
        Video <$>
            v .: "Codec"                            <*>
            v .: "CodecOptions"                     <*>
            v .: "KeyFrameRateDist"                 <*>
            v .: "FixedGOP"                         <*>
            v .: "BitRate"                          <*>
            v .: "FrameRate"                        <*>
            v .: "Resolution"                       <*>
            v .: "AspectRatio"
    parseJSON _          = mzero

instance ToJSON Video where
    toJSON vd@(Video _ _ _ _ _ _ _ _) =
        object 
            [ "Codec"            .= vdCodec            vd
            , "CodecOptions"     .= vdCodecOptions     vd
            , "KeyFrameRateDist" .= vdKeyFrameRateDist vd
            , "FixedGOP"         .= vdFixedGOP         vd
            , "BitRate"          .= vdBitRate          vd
            , "FrameRate"        .= vdFrameRate        vd
            , "Resolution"       .= vdResolution       vd
            , "AspectRatio"      .= vdAspectRatio      vd
            ]

instance QC.Arbitrary Video where
    arbitrary = Video
                    <$> QC.arbitrary 
                    <*> QC.arbitrary
                    <*> QC.arbitrary
                    <*> QC.arbitrary
                    <*> QC.arbitrary
                    <*> QC.arbitrary
                    <*> QC.arbitrary
                    <*> QC.arbitrary


--
-- | Thumbnails
--

data Thumbnails
    = Thumbnails
        { thFormat      :: ThumbnailFormat
        , thInterval    :: Interval
        , thResolution  :: Resolution
        , thAspectRatio :: AspectRatio
        }
    deriving (Show,Eq)

instance FromJSON Thumbnails where
    parseJSON (Object v) = 
        Thumbnails <$>
            v .: "thFormat"                           <*>
            v .: "thInterval"                         <*>
            v .: "thResolution"                       <*>
            v .: "thAspectRatio"
    parseJSON _          = mzero

instance ToJSON Thumbnails where
    toJSON th@(Thumbnails _ _ _ _) =
        object 
            [ "thFormat"      .= thFormat      th
            , "thInterval"    .= thInterval    th
            , "thResolution"  .= thResolution  th
            , "thAspectRatio" .= thAspectRatio th
            ]

instance QC.Arbitrary Thumbnails where
    arbitrary = Thumbnails
                    <$> QC.arbitrary 
                    <*> QC.arbitrary
                    <*> QC.arbitrary
                    <*> QC.arbitrary


--
-- | CodecOptions
--

data CodecOptions
    = CodecOptions
        { coProfile            :: Profile
        , coLevel              :: Level
        , coMaxReferenceFrames :: MaxReferenceFrames
        }
    deriving (Show,Eq)

instance FromJSON CodecOptions where
    parseJSON (Object v) = 
        CodecOptions <$>
            v .: "Profile"                            <*>
            v .: "Level"                              <*>
            v .: "MaxReferenceFrames"
    parseJSON _          = mzero

instance ToJSON CodecOptions where
    toJSON th@(CodecOptions _ _ _) =
        object 
            [ "Profile"            .= coProfile            th
            , "Level"              .= coLevel              th
            , "MaxReferenceFrames" .= coMaxReferenceFrames th
            ]

instance QC.Arbitrary CodecOptions where
    arbitrary = CodecOptions
                    <$> QC.arbitrary 
                    <*> QC.arbitrary
                    <*> QC.arbitrary


--
-- | AudioCodec
--

data AudioCodec
    = AC_AAC
    deriving (Show,Eq,Ord,Bounded,Enum)

acodec_t :: AudioCodec -> T.Text
acodec_t ac =
        case ac of
          AC_AAC -> "AAC"

acodec_m :: Map.Map T.Text AudioCodec
acodec_m = text_map acodec_t

instance FromJSON AudioCodec where
    parseJSON = json_str_map_p acodec_m

instance ToJSON AudioCodec where
    toJSON = String . acodec_t

instance QC.Arbitrary AudioCodec where
    arbitrary = QC.elements [minBound..maxBound]


--
-- | SampleRate
--

data SampleRate
    = SRauto
    | SR22050
    | SR32050
    | SR44100
    | SR48000
    | SR96000
    deriving (Show,Eq,Ord,Bounded,Enum)

srate_t :: SampleRate -> T.Text
srate_t sr =
    case sr of
      SRauto  -> "auto"
      SR22050 -> "22050"
      SR32050 -> "32050"
      SR44100 -> "44100"
      SR48000 -> "48000"
      SR96000 -> "96000"

srate_m :: Map.Map T.Text SampleRate
srate_m = text_map srate_t

instance FromJSON SampleRate where
    parseJSON = json_str_map_p srate_m

instance ToJSON SampleRate where
    toJSON = String . srate_t

instance QC.Arbitrary SampleRate where
    arbitrary = QC.elements [minBound..maxBound]


--
-- | Channels
--

data Channels
    = Chs_auto
    | Chs_0
    | Chs_1
    | Chs_2
    deriving (Show,Eq,Ord,Bounded,Enum)

channels_t :: Channels -> T.Text
channels_t ac =
    case ac of
      Chs_auto -> "auto"
      Chs_0 -> "0"
      Chs_1 -> "1"
      Chs_2 -> "2"

channels_m :: Map.Map T.Text Channels
channels_m = text_map channels_t

instance FromJSON Channels where
    parseJSON = json_str_map_p channels_m

instance ToJSON Channels where
    toJSON = String . channels_t

instance QC.Arbitrary Channels where
    arbitrary = QC.elements [minBound..maxBound]


--
-- | VideoCodec
--

data VideoCodec
    = VC_H_264
    deriving (Show,Eq,Ord,Bounded,Enum)

vcodec_t :: VideoCodec -> T.Text
vcodec_t ac =
        case ac of
          VC_H_264 -> "H.264"

vcodec_m :: Map.Map T.Text VideoCodec
vcodec_m = text_map vcodec_t

instance FromJSON VideoCodec where
    parseJSON = json_str_map_p vcodec_m

instance ToJSON VideoCodec where
    toJSON = String . vcodec_t

instance QC.Arbitrary VideoCodec where
    arbitrary = QC.elements [minBound..maxBound]


--
-- | ThumbnailFormat
--

data ThumbnailFormat
    = TF_png
    deriving (Show,Eq,Ord,Bounded,Enum)

thumb_t :: ThumbnailFormat -> T.Text
thumb_t ac =
        case ac of
          TF_png -> "png"

thumb_m :: Map.Map T.Text ThumbnailFormat
thumb_m = text_map thumb_t

instance FromJSON ThumbnailFormat where
    parseJSON = json_str_map_p thumb_m

instance ToJSON ThumbnailFormat where
    toJSON = String . thumb_t

instance QC.Arbitrary ThumbnailFormat where
    arbitrary = QC.elements [minBound..maxBound]


--
-- | Profile
--

data Profile
    = Pr_baseline
    | Pr_main
    | Pr_high
    deriving (Show,Eq,Ord,Bounded,Enum)

profile_t :: Profile -> T.Text
profile_t ac =
        case ac of
          Pr_baseline -> "baseline"
          Pr_main     -> "main"
          Pr_high     -> "high"

profile_m :: Map.Map T.Text Profile
profile_m = text_map profile_t

instance FromJSON Profile where
    parseJSON = json_str_map_p profile_m

instance ToJSON Profile where
    toJSON = String . profile_t

instance QC.Arbitrary Profile where
    arbitrary = QC.elements [minBound..maxBound]


--
-- | Level
--

data Level
    = Lv_1
    | Lv_1b
    | Lv_1_1
    | Lv_1_2
    | Lv_1_3
    | Lv_2
    | Lv_2_1
    | Lv_2_2
    | Lv_3
    | Lv_3_1
    | Lv_3_2
    | Lv_4
    | Lv_4_1
    deriving (Show,Eq,Ord,Bounded,Enum)

level_t :: Level -> T.Text
level_t ac =
    case ac of
      Lv_1   -> "1"
      Lv_1b  -> "1b"
      Lv_1_1 -> "1.1"
      Lv_1_2 -> "1.2"
      Lv_1_3 -> "1.3"
      Lv_2   -> "2"
      Lv_2_1 -> "2.1"
      Lv_2_2 -> "2.2"
      Lv_3   -> "3"
      Lv_3_1 -> "3.1"
      Lv_3_2 -> "3.2"
      Lv_4   -> "4"
      Lv_4_1 -> "4.1"

level_m :: Map.Map T.Text Level
level_m = text_map level_t

instance FromJSON Level where
    parseJSON = json_str_map_p level_m

instance ToJSON Level where
    toJSON = String . level_t

instance QC.Arbitrary Level where
    arbitrary = QC.elements [minBound..maxBound]


--
-- | PresetType
--

data PresetType
    = PTcustom
    | PTsystem
    deriving (Show,Eq,Ord,Bounded,Enum)

prtype_t :: PresetType -> T.Text
prtype_t pt =
        case pt of
          PTcustom -> "custom"
          PTsystem -> "system"

prtype_m :: Map.Map T.Text PresetType
prtype_m = text_map prtype_t

instance FromJSON PresetType where
    parseJSON = json_str_map_p prtype_m

instance ToJSON PresetType where
    toJSON = String . prtype_t

instance QC.Arbitrary PresetType where
    arbitrary = QC.elements [minBound..maxBound]


--
-- | BitRate
--

newtype BitRate = KbPerSec { _KbPerSec :: Int }
    deriving (Show,Eq,Ord,Bounded,Enum)

instance FromJSON BitRate where
    parseJSON = withText "BitRate" $ \t -> KbPerSec <$> read_p t

instance ToJSON BitRate where
    toJSON = String . T.pack . show . _KbPerSec

instance QC.Arbitrary BitRate where
    arbitrary = KbPerSec <$> QC.arbitrary


--
-- | KeyFrameRate
--

newtype KeyFrameRate = KeyFrameRate { _KeyFrameRate :: Int }
    deriving (Show,Eq)

instance FromJSON KeyFrameRate where
    parseJSON = withText "KeyFrameRate" $ \t -> KeyFrameRate <$> read_p t

instance ToJSON KeyFrameRate where
    toJSON = String . T.pack . show . _KeyFrameRate

instance QC.Arbitrary KeyFrameRate where
    arbitrary = KeyFrameRate <$> QC.arbitrary


--
-- | FixedGOP
--

newtype FixedGOP = FixedGOP { _FixedGOP :: Bool }
    deriving (Show,Eq)

instance FromJSON FixedGOP where
    parseJSON = withText "FixedGOP" $ \t -> FixedGOP <$> read_p t

instance ToJSON FixedGOP where
    toJSON = String . T.pack . show . _FixedGOP

instance QC.Arbitrary FixedGOP where
    arbitrary = FixedGOP <$> QC.arbitrary


--
-- | Interval
--

newtype Interval = Interval { _Interval :: Int }
    deriving (Show,Eq)

instance FromJSON Interval where
    parseJSON = withText "Interval" $ \t -> Interval <$> read_p t

instance ToJSON Interval where
    toJSON = String . T.pack . show . _Interval

instance QC.Arbitrary Interval where
    arbitrary = Interval <$> QC.arbitrary


--
-- | MaxReferenceFrames
--

newtype MaxReferenceFrames = MaxReferenceFrames
                                            { _MaxReferenceFrames :: Int }
    deriving (Show,Eq)

instance FromJSON MaxReferenceFrames where
    parseJSON = withText "MaxReferenceFrames" 
                                        $ \t -> MaxReferenceFrames <$> read_p t

instance ToJSON MaxReferenceFrames where
    toJSON = String . T.pack . show . _MaxReferenceFrames

instance QC.Arbitrary MaxReferenceFrames where
    arbitrary = MaxReferenceFrames <$> QC.arbitrary


--
-- | MessagesSuccess
--

data MessagesSuccess
    = MessagesSuccess
        { msMessages :: [T.Text]
        , msSuccess  :: Bool
        }
    deriving (Show,Eq)

instance FromJSON MessagesSuccess where
    parseJSON (Object v) = 
        MessagesSuccess <$>
            v .: "Messages"                         <*>
            v .: "Success"
    parseJSON _          = mzero

instance ToJSON MessagesSuccess where
    toJSON rt@(MessagesSuccess _ _) =
        object 
            [ "Messages" .= msMessages rt
            , "Success"  .= msSuccess  rt
            ]

instance QC.Arbitrary MessagesSuccess where
    arbitrary = MessagesSuccess
                    <$> (map T.pack <$> QC.arbitrary) 
                    <*>                 QC.arbitrary


--
-- | 'auto', 'true' or 'false'
--

data AutoBool
    = ABauto    
    | ABtrue
    | ABfalse
    deriving (Show,Eq,Ord,Bounded,Enum)

autobool_t :: AutoBool -> T.Text
autobool_t rot =
    case rot of
      ABauto    -> "auto"
      ABtrue    -> "true"
      ABfalse   -> "false"

autobool_m :: Map.Map T.Text AutoBool
autobool_m = text_map autobool_t

instance FromJSON AutoBool where
    parseJSON = json_str_map_p autobool_m

instance ToJSON AutoBool where
    toJSON = String . autobool_t

instance QC.Arbitrary AutoBool where
    arbitrary = QC.elements [minBound..maxBound]



--
-- | Text or Null
--

data TextOrNull
    = TNText T.Text
    | TNNull
    deriving (Show,Eq) 

instance IsString TextOrNull where
    fromString = TNText . T.pack

instance FromJSON TextOrNull where
    parseJSON Null       = return   TNNull 
    parseJSON (String t) = return $ TNText t
    parseJSON _          = mzero

instance ToJSON TextOrNull where
    toJSON  TNNull    = Null
    toJSON (TNText t) = String t

instance QC.Arbitrary TextOrNull where
    arbitrary = maybe TNNull TNText <$> (poss $ T.pack <$> QC.arbitrary)


--
-- | Ets error message
--

newtype EtsServiceError = ESE { _ESE :: T.Text }
    deriving (Show,IsString,Eq)

instance FromJSON EtsServiceError where
    parseJSON (Object v) = ESE <$> v .: "message"
    parseJSON _          = mzero

instance ToJSON EtsServiceError where
    toJSON (ESE msg) =
        object 
            [ "message" .= msg
            ]

instance QC.Arbitrary EtsServiceError where
    arbitrary = ESE . T.pack <$> QC.arbitrary



--
-- | 'success'
--

newtype SUCCESS = SUCCESS { _SUCCESS :: Bool }
    deriving (Show,Eq)

instance FromJSON SUCCESS where
    parseJSON (Object v) = SUCCESS <$> v .: "success"
    parseJSON _          = mzero

instance ToJSON SUCCESS where
    toJSON = Bool . _SUCCESS

instance QC.Arbitrary SUCCESS where
    arbitrary = SUCCESS <$> QC.arbitrary



--
-- | 'true', 'false'
--


bool_t :: Bool -> T.Text
bool_t True  = "true"
bool_t False = "false"



------------------------------------------------------------------------------
--
-- Parser Toolkit
--
------------------------------------------------------------------------------


json_str_map_p :: Ord a => Map.Map T.Text a -> Value -> Parser a
json_str_map_p mp = json_string_p $ flip Map.lookup mp 

json_string_p :: Ord a => (T.Text->Maybe a) -> Value -> Parser a
json_string_p p (String t) | Just val <- p t = return val
                           | otherwise       = mzero
json_string_p _  _                           = mzero

text_map :: (Ord a,Bounded a,Enum a) => (a->T.Text) -> Map.Map T.Text a
text_map f = Map.fromList [ (f x,x) | x<-[minBound..maxBound] ]

read_p :: Read a => T.Text -> Parser a
read_p txt = maybe mzero return $ readMay $ T.unpack txt


------------------------------------------------------------------------------
--
-- QC Toolkit
--
------------------------------------------------------------------------------


poss :: QC.Gen a -> QC.Gen (Maybe a)
poss gen = QC.frequency 
    [ (,) 1  $ QC.elements [Nothing]
    , (,) 20 $ Just <$> gen
    ]

nat_pair :: QC.Gen (Int,Int)
nat_pair = two $ QC.sized $ \n -> QC.choose (0, n)

two :: QC.Gen a -> QC.Gen (a,a)
two gen = (,) <$> gen <*> gen
