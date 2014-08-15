module Network.XHR where

import Control.Monad.Eff
import qualified Network.XHR.Internal as I

import Data.Maybe
import Data.Foldable
import Data.Tuple

data Body r
    = NoBody
    | UrlEncoded {|r}
    | Multipart  {|r}

type AjaxOptions r =
    { method      :: String
    , url         :: String
    , headers     :: [Tuple String String]
    , cache       :: Boolean
    , timeout     :: Number
    , credentials :: Boolean

    , async       :: Boolean
    , user        :: String
    , password    :: String
    
    , onAbort            :: Response -> I.EffAjax r Unit
    , onError            :: Response -> I.EffAjax r Unit
    , onLoad             :: Response -> I.EffAjax r Unit
    , onLoadEnd          :: Response -> I.EffAjax r Unit
    , onProgress         :: Response -> I.EffAjax r Unit
    , onReadyStateChange :: I.ReadyState -> Response -> I.EffAjax r Unit
    , onTimeout          :: Response -> I.EffAjax r Unit
    }

newtype Response = Response I.XHR
getAllResponseHeaders :: forall r. Response -> I.EffAjax r String
getAllResponseHeaders (Response xhr) = I.getAllResponseHeaders xhr

getResponseHeader :: forall r. String -> Response -> I.EffAjax r String
getResponseHeader k   (Response xhr) = I.getResponseHeader k xhr

getReadyState :: forall r. Response -> I.EffAjax r I.ReadyState
getReadyState         (Response xhr) = I.getReadyState xhr

getResponseText :: forall r. Response -> I.EffAjax r String
getResponseText       (Response xhr) = I.getResponseText xhr

getResponseXML :: forall r. Response -> I.EffAjax r String
getResponseXML        (Response xhr) = I.getResponseXML xhr

getStatus :: forall r. Response -> I.EffAjax r Number
getStatus             (Response xhr) = I.getStatus xhr

getStatusText :: forall r. Response -> I.EffAjax r String
getStatusText         (Response xhr) = I.getStatusText xhr

defaultAjaxOptions :: forall r. AjaxOptions r
defaultAjaxOptions =
    { method: "GET"
    , url: "/"
    , headers: []
    , cache: true
    , timeout: 0
    , credentials: false

    , async: true
    , user: ""
    , password: ""

    , onAbort:            \_ ->   return unit
    , onError:            \_ ->   return unit
    , onLoad:             \_ ->   return unit
    , onLoadEnd:          \_ ->   return unit
    , onProgress:         \_ ->   return unit
    , onReadyStateChange: \_ _ -> return unit
    , onTimeout:          \_ ->   return unit
    }

ajax :: forall r a b. AjaxOptions r -> {|a} -> Body b -> I.EffAjax r Unit
ajax conf params body = do
    xhr <- I.newXMLHttpRequest
    I.open openConfig xhr
    -- set props
    I.setTimeout conf.timeout xhr
    I.setWithCredentials conf.credentials xhr
    I.setOnAbort            (conf.onAbort    (Response xhr)) xhr
    I.setOnError            (conf.onError    (Response xhr)) xhr
    I.setOnLoad             (conf.onLoad     (Response xhr)) xhr
    I.setOnLoadEnd          (conf.onLoadEnd  (Response xhr)) xhr
    I.setOnProgress         (conf.onProgress (Response xhr)) xhr
    I.setOnTimeout          (conf.onTimeout  (Response xhr)) xhr
    I.setOnReadyStateChange ( do 
        st <- I.getReadyState xhr
        conf.onReadyStateChange st (Response xhr)
        ) xhr

    -- set headers
    for_ (headers :: [Tuple String String]) $ \(Tuple k v) ->
        I.setRequestHeader k v xhr

    case body of
        NoBody       -> I.send xhr
        UrlEncoded b -> do
            I.setRequestHeader "Content-Type" "application/x-www-form-urlencoded" xhr
            I.sendWithBody (I.encodeUrlParams b) xhr
        Multipart  b ->
            I.sendWithBody (I.encodeMultipart b) xhr

  where
    paramString = I.encodeUrlParams params
    url = if paramString == "" then conf.url else conf.url ++ "?" ++ paramString
    openConfig = I.defaultOpenConfig { method   = conf.method
                                     , url      = url
                                     , async    = conf.async
                                     , user     = conf.user
                                     , password = conf.password
                                     }
    headers = if conf.cache
                  then conf.headers
                  else Tuple "Pragma" "no-cache":
                       Tuple "Cache-Control" "no-cache":
                       Tuple "IF-Modified-Since" "Thu, 01 Jun 1970 00:00:00 GMT":
                       conf.headers
