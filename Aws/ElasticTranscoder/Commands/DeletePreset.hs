{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 

module Aws.ElasticTranscoder.Commands.DeletePreset
    ( DeletePreset(..)
    , DeletePresetResponse(..)
    ) where

import           Aws.Core
import           Aws.ElasticTranscoder.Core
import           Control.Applicative
import           Data.Aeson
import           Data.Text                      as T


data DeletePreset
    = DeletePreset
        { dprId :: PresetId
        }
    deriving (Show,Eq)

data DeletePresetResponse
    = DeletePresetResponse 
    deriving (Show,Eq)

instance SignQuery DeletePreset where

    type ServiceConfiguration DeletePreset = EtsConfiguration

    signQuery DeletePreset{..} = etsSignQuery 
        EtsQuery
            { etsqMethod  = Delete
            , etsqRequest = "presets/" `T.append` _PresetId dprId
            , etsqQuery   = []
            , etsqBody    = Nothing
            }

instance ResponseConsumer DeletePreset DeletePresetResponse where

    type ResponseMetadata DeletePresetResponse = EtsMetadata

    responseConsumer _ mref = etsResponseConsumer mref $ \rsp ->
                                                    cnv <$> jsonConsumer rsp
          where
            cnv :: Value -> DeletePresetResponse
            cnv _ = DeletePresetResponse

instance Transaction DeletePreset DeletePresetResponse

instance AsMemoryResponse DeletePresetResponse where

    type MemoryResponse DeletePresetResponse = DeletePresetResponse

    loadToMemory = return
