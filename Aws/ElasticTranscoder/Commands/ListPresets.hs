{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 

module Aws.ElasticTranscoder.Commands.ListPresets
    ( ListPresets(..)
    , ListPresetsResponse(..)
    ) where

import           Aws.Core
import           Aws.ElasticTranscoder.Core
import           Control.Applicative


data ListPresets
    = ListPresets
    deriving (Show,Eq)

newtype ListPresetsResponse
    = ListPresetsResponse
        { lprPresets      :: [PresetResponse]
        }
    deriving (Show,Eq)

instance SignQuery ListPresets where

    type ServiceConfiguration ListPresets = EtsConfiguration

    signQuery ListPresets{..} = etsSignQuery 
        EtsQuery
            { etsqMethod  = Get
            , etsqRequest = "presets"
            , etsqQuery   = []
            , etsqBody    = Nothing
            }

instance ResponseConsumer ListPresets ListPresetsResponse where

    type ResponseMetadata ListPresetsResponse = EtsMetadata

    responseConsumer _ mref = etsResponseConsumer mref $ \rsp ->
                                                    cnv <$> jsonConsumer rsp
          where
            cnv (PresetList pls) = ListPresetsResponse pls

instance Transaction ListPresets ListPresetsResponse

instance AsMemoryResponse ListPresetsResponse where

    type MemoryResponse ListPresetsResponse = ListPresetsResponse

    loadToMemory = return
