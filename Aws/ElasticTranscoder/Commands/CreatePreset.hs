{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 

module Aws.ElasticTranscoder.Commands.CreatePreset
    ( CreatePreset(..)
    , CreatePresetResponse(..)
    ) where

import           Aws.Core
import           Aws.ElasticTranscoder.Core
import           Control.Applicative
import           Data.Aeson
import qualified Data.Text                      as T


data CreatePreset
    = CreatePreset
        { cptName        :: PresetName
        , cptDescription :: T.Text
        , cptContainer   :: Container
        , cptAudio       :: Audio
        , cptVideo       :: Video
        , cptThumbnails  :: Thumbnails
        }
    deriving (Show,Eq)

data CreatePresetResponse
    = CreatePresetResponse
        { cptrName        :: PresetName
        , cptrDescription :: T.Text
        , cptrContainer   :: Container
        , cptrAudio       :: Audio
        , cptrVideo       :: Video
        , cptrThumbnails  :: Thumbnails
        , cptrId          :: PresetId
        , cptrType        :: PresetType
        , cptrWarning     :: T.Text
        }
    deriving (Show,Eq)

instance SignQuery CreatePreset where

    type ServiceConfiguration CreatePreset = EtsConfiguration

    signQuery CreatePreset{..} = etsSignQuery 
        EtsQuery
            { etsqMethod  = Post
            , etsqRequest = "presets"
            , etsqQuery   = []
            , etsqBody    = Just $ toJSON $ 
                                Preset
                                    cptName 
                                    cptDescription
                                    cptContainer
                                    cptAudio
                                    cptVideo
                                    cptThumbnails
            }

instance ResponseConsumer CreatePreset CreatePresetResponse where

    type ResponseMetadata CreatePresetResponse = EtsMetadata

    responseConsumer _ mref = etsResponseConsumer mref $ \rsp ->
                                                    cnv <$> jsonConsumer rsp
          where
            cnv (PresetSingle(PresetResponse a b c d e f g h i)) = 
                                        CreatePresetResponse a b c d e f g h i

instance Transaction CreatePreset CreatePresetResponse

instance AsMemoryResponse CreatePresetResponse where

    type MemoryResponse CreatePresetResponse = CreatePresetResponse

    loadToMemory = return
