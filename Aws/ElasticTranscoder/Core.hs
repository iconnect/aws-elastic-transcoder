{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-} 
{-# LANGUAGE RecordWildCards            #-} 
{-# LANGUAGE FlexibleInstances          #-} 
{-# LANGUAGE DeriveDataTypeable         #-} 

module Aws.ElasticTranscoder.Core
    ( EtsQuery(..)
    , EtsConfiguration(..)
    , etsConfiguration
    , ETSEndpoint
    , etsEndpointUsEast
    , etsEndpointUsWest
    , etsEndpointUsWest2
    , etsEndpointEu
    , etsEndpointApSouthEast
    , etsEndpointApNorthEast   
    , endpoint 
    , region
    , EtsError(..)
    , EtsMetadata(..)
    , etsSignQuery
    , etsResponseConsumer
    , jsonConsumer
    , module Aws.Core
    , module Aws.ElasticTranscoder.Json.Types
    ) where

import           Aws.Sign4
import           Aws.Core
import           Aws.ElasticTranscoder.Json.Types
import qualified Control.Exception              as C
import           Control.Monad
import           Control.Applicative
import           Control.Monad.IO.Class
import           Text.Printf
import           Data.String
import           Data.Monoid
import           Data.Aeson
import           Data.Time
import           Data.IORef
import           Data.Maybe
import           Data.Typeable
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Conduit                   as C
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as BC
import qualified Data.ByteString.Lazy.Char8     as LC
import qualified Network.HTTP.Conduit           as HTTP
import qualified Network.HTTP.Types             as HTTP

--import           Crypto.Hash.SHA256

data EtsQuery
    = EtsQuery
        { etsqMethod  :: Method
        , etsqRequest :: T.Text
        , etsqQuery   :: HTTP.Query
        , etsqBody    :: Maybe Value
        } deriving (Show)

data EtsConfiguration qt
    = EtsConfiguration
        { etsProtocol      :: Protocol
        , etsEndpoint      :: ETSEndpoint
        , etsPort          :: Int
        , etsDefaultExpiry :: NominalDiffTime
        }
    deriving (Show)

instance DefaultServiceConfiguration (EtsConfiguration NormalQuery) where
    defServiceConfig   = etsConfiguration HTTPS etsEndpointUsEast
    debugServiceConfig = etsConfiguration HTTP  etsEndpointUsEast

newtype ETSEndpoint = ETSEndpoint { _ETSEndpoint :: B.ByteString }
    deriving (Show)

instance IsString ETSEndpoint where
    fromString = ETSEndpoint . BC.pack

etsConfiguration :: Protocol -> ETSEndpoint -> EtsConfiguration qt
etsConfiguration pro edp = 
    EtsConfiguration 
        { etsProtocol      = pro
        , etsEndpoint      = edp
        , etsPort          = defaultPort pro
        , etsDefaultExpiry = 15*60
        }

etsEndpointUsEast, etsEndpointUsWest, etsEndpointUsWest2, etsEndpointEu,
                etsEndpointApSouthEast, etsEndpointApNorthEast :: ETSEndpoint

etsEndpointUsEast      = "us-east-1"
etsEndpointUsWest      = "us-west-1"
etsEndpointUsWest2     = "us-west-2"
etsEndpointEu          = "eu-west-1"
etsEndpointApSouthEast = "ap-southeast-1"
etsEndpointApNorthEast = "ap-northeast-1"


endpoint, region :: ETSEndpoint -> B.ByteString

endpoint = \edp -> B.concat ["elastictranscoder.",region edp,".amazonaws.com"]
region   = _ETSEndpoint


data EtsError
    = EtsError
        { etsStatusCode   :: HTTP.Status
        , etsErrorMessage :: T.Text
        }

     -- { etsError        :: B.ByteString
     -- }
    
    deriving (Show, Typeable)

instance C.Exception EtsError


data EtsMetadata
    = EtsMetadata 
        { etsMAmzId2    :: Maybe T.Text
        , etsMRequestId :: Maybe T.Text
        }
    deriving (Show, Typeable)

instance Monoid EtsMetadata where
    mempty        = EtsMetadata Nothing Nothing
    mappend m1 m2 = EtsMetadata (a1 `mplus` a2) (r1 `mplus` r2)
      where
        EtsMetadata a1 r1 = m1
        EtsMetadata a2 r2 = m2

instance Loggable EtsMetadata where
    toLogText (EtsMetadata id2 rid) = 
        "S3: request ID=" 
                `mappend` fromMaybe "<none>" rid
                `mappend` ", x-amz-id-2=" 
                `mappend` fromMaybe "<none>" id2


etsSignQuery :: EtsQuery -> EtsConfiguration qt -> SignatureData -> SignedQuery
etsSignQuery EtsQuery{..} EtsConfiguration{..} SignatureData{..} =
    SignedQuery
        { sqMethod        = etsqMethod
        , sqProtocol      = etsProtocol
        , sqHost          = endpoint etsEndpoint
        , sqPort          = etsPort
        , sqPath          = pth
        , sqQuery         = etsqQuery
        , sqDate          = Just signatureTime
        , sqAuthorization = Just aut 
        , sqBody          = HTTP.RequestBodyLBS <$> lbd
        , sqStringToSign  = sts                 -- NB for debugging only
        , sqContentType   = ctp
        , sqContentMd5    = Nothing
        , sqAmzHeaders    = []
        , sqOtherHeaders  = hdd
        }


  where
    -- authorization (and string to sign) fields

    aut = s4Authz        sg4    
    sts = s4StringToSign sg4

    -- AWS Signature v4 parameters
     
    sg4 =
        Sign4
            { s4Credentials = signatureCredentials
            , s4Date        = signatureTime
            , s4Endpoint    = region etsEndpoint
            , s4Service     = "elastictranscoder"
            , s4Method      = mth
            , s4Path        = pth
            , s4Headers     = hds
            , s4Query       = etsqQuery
            , s4Body        = maybe B.empty id bdy
            , s4SgndHeaders = Nothing
            , s4CnclHeaders = Nothing
            }
    
    -- the headers (with and without 'host' header)
    
    hds =
        [ (,) "Host" $ endpoint etsEndpoint
        ] ++ hdd

    hdd = 
        [ (,) "Date" $ fmtTime iso8601BasicUtcDate signatureTime
        ]
    
    -- URI path

    pth = BC.pack $ printf "/2012-09-25/%s" $ T.unpack etsqRequest

    -- method, content type and body
    
    mth = 
        case etsqMethod of
        --Head      -> "HEAD" 
          Get       -> "GET"
          PostQuery -> "POST"
          Post      -> "POST"
          Put       -> "PUT"
          Delete    -> "DELETE"

    ctp = case etsqMethod of
            Post -> Just "application/json; charset=UTF-8"
            _    -> Nothing
    
    bdy = BC.pack . LC.unpack <$> lbd
    lbd = encode <$> etsqBody
    
etsResponseConsumer :: IORef EtsMetadata -> HTTPResponseConsumer a ->
                                                        HTTPResponseConsumer a
etsResponseConsumer mrf inr rsp = 
 do liftIO $ tellMetadataRef mrf
                EtsMetadata 
                    { etsMAmzId2    = ai2
                    , etsMRequestId = rqi
                    }
    if HTTP.responseStatus rsp >= HTTP.status400
      then ets_error_rc rsp     -- handle error
      else inr          rsp     -- normal processing
  where
    ai2 = mhs "x-amz-id-2"
    rqi = mhs "x-amz-request-id"

    -- extract header string
    
    mhs = fmap T.decodeUtf8 . flip lookup (HTTP.responseHeaders rsp)

ets_error_rc :: HTTPResponseConsumer a
ets_error_rc rsp0 =
 do rsp <- HTTP.lbsResponse rsp0
    C.monadThrow $ err rsp $ HTTP.responseBody rsp
  where
    err rsp msg = 
            case eitherDecode msg :: Either String EtsServiceError of
              Left per -> 
                EtsError
                    { etsStatusCode   = HTTP.responseStatus rsp
                    , etsErrorMessage = oops per msg
                    }
              Right ese -> 
                EtsError
                    { etsStatusCode   = HTTP.responseStatus rsp
                    , etsErrorMessage = _ESE ese
                    }

    oops per msg =
                T.pack $ printf "JSON parse error (%s): %s" per $ LC.unpack msg

jsonConsumer :: FromJSON a => HTTPResponseConsumer a
jsonConsumer rsp0 =
 do rsp <- HTTP.lbsResponse rsp0
    either (C.monadThrow . oops rsp) return $ eitherDecode $ HTTP.responseBody rsp 
  where
    oops rsp dgc = 
        EtsError
            { etsStatusCode   = HTTP.responseStatus rsp
            , etsErrorMessage = "Failed to parse JSON response: " `T.append` T.pack dgc
            }
