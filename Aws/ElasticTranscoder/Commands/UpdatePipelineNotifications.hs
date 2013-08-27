{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE TypeFamilies               #-} 
{-# LANGUAGE MultiParamTypeClasses      #-} 
{-# LANGUAGE OverloadedStrings          #-} 

module Aws.ElasticTranscoder.Commands.UpdatePipelineNotifications
    ( UpdatePipelineNotifications(..)
    ) where

import           Aws.Core
import           Aws.ElasticTranscoder.Core
import           Control.Applicative
import           Data.Aeson
import           Data.Text                      as T


data UpdatePipelineNotifications
    = UpdatePipelineNotifications
        { upnId            :: PipelineId
        , upnNotifications :: Notifications
        }
    deriving (Show,Eq)

instance SignQuery UpdatePipelineNotifications where

    type ServiceConfiguration UpdatePipelineNotifications = EtsConfiguration

    signQuery UpdatePipelineNotifications{..} = etsSignQuery 
        EtsQuery
            { etsqMethod  = Get
            , etsqRequest = "pipelines/" `T.append` _PipelineId upnId
                                         `T.append` "/notifications"
            , etsqQuery   = []
            , etsqBody    = Just $ toJSON $ PipelineIdAndNotifications upnId upnNotifications
            }

instance ResponseConsumer UpdatePipelineNotifications UpdatePipelineNotifications 
                                                                        where

    type ResponseMetadata UpdatePipelineNotifications = EtsMetadata

    responseConsumer _ mref = etsResponseConsumer mref $ \rsp ->
                                                    cnv <$> jsonConsumer rsp
          where
            cnv (PipelineIdAndNotifications a b) = UpdatePipelineNotifications a b

instance Transaction UpdatePipelineNotifications UpdatePipelineNotifications

instance AsMemoryResponse UpdatePipelineNotifications where

    type MemoryResponse UpdatePipelineNotifications = 
                                                UpdatePipelineNotifications

    loadToMemory = return
