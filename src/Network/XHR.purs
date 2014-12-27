module Network.XHR
    ( multipart, urlEncoded, noBody, rawBody
    
    , AjaxOptions(..), Response(), URL(..)
    , EffAjax(..), Query(), Callback(), XHRTask()
    , HasReadyState

    , abort

    , getAllResponseHeaders, getResponseHeader
    , getReadyState
    , getResponseText, getResponseXML
    , getStatus, getStatusText

    , defaultAjaxOptions, ajax

    , get, post

    , onUnsent, onOpened, onHeaderReceived
    , onLoading, onDone, onSuccess

    , unsafeToResponse
    ) where

import Control.Monad.Eff
import qualified Network.XHR.Internal as I
import Network.XHR.Types

import Data.Maybe
import Data.Foldable
import Data.Tuple

type EffAjax r = Eff (ajax :: I.Ajax | r)

type URL = String

type Query a = {|a}

type Callback r = Response -> EffAjax r Unit

multipart :: forall a. {|a} -> Body I.FormData
multipart a = Multipart $ I.encodeMultipart a

urlEncoded :: forall a. {|a} -> Body String
urlEncoded a = UrlEncoded $ I.encodeUrlParams a

noBody :: forall a. Body a
noBody = NoBody

rawBody :: String -> Body String
rawBody = RawBody

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
    
    , onAbort            :: Callback r
    , onError            :: Callback r
    , onLoad             :: Callback r
    , onLoadEnd          :: Callback r
    , onProgress         :: Callback r
    , onReadyStateChange :: Callback r
    , onTimeout          :: Callback r
    }

newtype Response = Response I.XHR
getAllResponseHeaders :: forall r. Response -> EffAjax r String
getAllResponseHeaders (Response xhr) = I.getAllResponseHeaders xhr

getResponseHeader :: forall r. String -> Response -> EffAjax r String
getResponseHeader k   (Response xhr) = I.getResponseHeader k xhr

getResponseText :: forall r. Response -> EffAjax r String
getResponseText       (Response xhr) = I.getResponseText xhr

getResponseXML :: forall r. Response -> EffAjax r (Maybe I.XML)
getResponseXML        (Response xhr) = I.getResponseXML xhr

getStatus :: forall r. Response -> EffAjax r Number
getStatus             (Response xhr) = I.getStatus xhr

getStatusText :: forall r. Response -> EffAjax r String
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

    , onAbort:            \_ -> return unit
    , onError:            \_ -> return unit
    , onLoad:             \_ -> return unit
    , onLoadEnd:          \_ -> return unit
    , onProgress:         \_ -> return unit
    , onReadyStateChange: \_ -> return unit
    , onTimeout:          \_ -> return unit
    }

newtype XHRTask = XHRTask I.XHR

abort :: forall r. XHRTask -> EffAjax r Unit
abort (XHRTask x) = I.abort x

unsafeToResponse :: XHRTask -> Response
unsafeToResponse (XHRTask x) = Response x

class HasReadyState a where
    getReadyState :: forall r. a -> EffAjax r ReadyState

instance hasReadyStateResponse :: HasReadyState Response where
    getReadyState (Response r) = I.getReadyState r

instance hasReadyStateXHRTask :: HasReadyState XHRTask where
    getReadyState (XHRTask r) = I.getReadyState r

ajax :: forall r a b. AjaxOptions r -> Query a -> Body b -> EffAjax r XHRTask
ajax conf params body = do
    xhr <- I.newXMLHttpRequest
    I.open openConfig xhr
    -- set props
    if conf.async
        then do
            I.setTimeout conf.timeout xhr
            I.setWithCredentials conf.credentials xhr
        else return unit
    I.setOnAbort            (conf.onAbort    (Response xhr)) xhr
    I.setOnError            (conf.onError    (Response xhr)) xhr
    I.setOnLoad             (conf.onLoad     (Response xhr)) xhr
    I.setOnLoadEnd          (conf.onLoadEnd  (Response xhr)) xhr
    I.setOnProgress         (conf.onProgress (Response xhr)) xhr
    I.setOnTimeout          (conf.onTimeout  (Response xhr)) xhr
    I.setOnReadyStateChange ( do 
        conf.onReadyStateChange (Response xhr)
        ) xhr

    -- set headers
    for_ (headers :: [Tuple String String]) $ \(Tuple k v) ->
        I.setRequestHeader k v xhr

    case body of
        NoBody       -> I.send xhr
        RawBody    b ->
            I.sendWithBody b xhr
        UrlEncoded b -> do
            I.setRequestHeader "Content-Type" "application/x-www-form-urlencoded" xhr
            I.sendWithBody b xhr
        Multipart  b ->
            I.sendWithBody b xhr

    return (XHRTask xhr)

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

get :: forall r a. AjaxOptions r -> URL -> Query a -> EffAjax r XHRTask
get c u p = ajax c { method = "GET", url = u } p NoBody

post :: forall r a b. AjaxOptions r -> URL -> Query a -> Body b -> EffAjax r XHRTask
post conf u = ajax conf { method = "POST", url = u }

onUnsent :: forall r. Callback r -> Callback r
onUnsent act res@(Response xhr) = do
    rs <- I.getReadyState' xhr
    if rs == 0
        then act res
        else return unit

onOpened :: forall r. Callback r -> Callback r
onOpened act res@(Response xhr) = do
    rs <- I.getReadyState' xhr
    if rs == 1
        then act res
        else return unit

onHeaderReceived :: forall r. Callback r -> Callback r
onHeaderReceived act res@(Response xhr) = do
    rs <- I.getReadyState' xhr
    if rs == 2
        then act res
        else return unit

onLoading :: forall r. Callback r -> Callback r
onLoading act res@(Response xhr) = do
    rs <- I.getReadyState' xhr
    if rs == 3
        then act res
        else return unit

onDone :: forall r. Callback r -> Callback r
onDone act res@(Response xhr) = do
    rs <- I.getReadyState' xhr
    if rs == 4
        then act res
        else return unit

onSuccess :: forall r. Callback r -> Callback r
onSuccess act res@(Response xhr) = do
    rs <- I.getReadyState' xhr
    st <- getStatus res
    if rs == 4 && st == 200
        then act res
        else return unit
