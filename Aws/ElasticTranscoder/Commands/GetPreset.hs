{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 

module Aws.ElasticTranscoder.Commands.GetPreset
    ( GetPreset(..)
    , GetPresetResponse(..)
    ) where

import           Aws.Core
import           Aws.ElasticTranscoder.Core
import           Control.Applicative
import qualified Data.Text                      as T


data GetPreset
    = GetPreset
        { gprPreset :: PresetId
        }
    deriving (Show,Eq)

data GetPresetResponse
    = GetPresetResponse
        { gprrName        :: PresetName
        , gprrDescription :: T.Text
        , gprrContainer   :: Container
        , gprrAudio       :: Audio
        , gprrVideo       :: Video
        , gprrThumbnails  :: Thumbnails
        , gprrId          :: PresetId
        , gprrType        :: PresetType
        , gprrWarning     :: T.Text
        }
    deriving (Show,Eq)

instance SignQuery GetPreset where

    type ServiceConfiguration GetPreset = EtsConfiguration

    signQuery GetPreset{..} = etsSignQuery 
        EtsQuery
            { etsqMethod  = Get
            , etsqRequest = "presets/" `T.append` _PresetId gprPreset
            , etsqQuery   = []
            , etsqBody    = Nothing
            }

instance ResponseConsumer GetPreset GetPresetResponse where

    type ResponseMetadata GetPresetResponse = EtsMetadata

    responseConsumer _ mref = etsResponseConsumer mref $ \rsp ->
                                                    cnv <$> jsonConsumer rsp
          where
            cnv (PresetSingle(PresetResponse a b c d e f g h i)) = 
                                            GetPresetResponse a b c d e f g h i

instance Transaction GetPreset GetPresetResponse

instance AsMemoryResponse GetPresetResponse where

    type MemoryResponse GetPresetResponse = GetPresetResponse

    loadToMemory = return
