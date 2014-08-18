module Network.XHR.Internal
    ( Ajax(..), EffAjax(..), OpenConfig(..)
    , XHR(..), FormData(..)

    , newXMLHttpRequest
    , open, defaultOpenConfig
    , send, sendWithBody
    , abort

    , getAllResponseHeaders, getResponseHeader
    , getReadyState, getReadyState'
    , getResponseText, getResponseXML
    , getStatus, getStatusText

    , setRequestHeader
    , setTimeout
    , setWithCredentials
    , overrideMimeType

    , setOnAbort
    , setOnError
    , setOnLoad
    , setOnLoadEnd
    , setOnProgress
    , setOnTimeout
    , setOnReadyStateChange

    , encodeUrlParams
    , encodeMultipart
    ) where

import Control.Monad.Eff
import Data.Function
import Data.Maybe
import Network.XHR.Types

foreign import data XHR  :: *
foreign import data FormData :: *
foreign import data Ajax :: !

type EffAjax r = Eff (ajax :: Ajax | r)

foreign import newXMLHttpRequest "\
    \function newXMLHttpRequest() {\
    \   try {\
    \       return new ActiveXObject('Msxml2.XMLHTTP');\
    \   } catch (e) {\
    \       try {\
    \           return new ActiveXObject('Microsoft.XMLHTTP');\
    \       } catch (e) {\
    \           return new XMLHttpRequest();\
    \       }\
    \   }\
    \}" :: forall r. EffAjax r XHR

foreign import abort "\
    \function abort(xhr) {\
    \   return function() {\
    \       xhr.abort();\
    \   }\
    \}" :: forall r. XHR -> EffAjax r Unit

type OpenConfig = { method   :: String
                  , url      :: String
                  , async    :: Boolean
                  , user     :: String
                  , password :: String
                  }

defaultOpenConfig :: OpenConfig
defaultOpenConfig = { method: "GET", url: "/", async: true, user: "", password: ""}

foreign import openImpl "\
    \function openImpl (config, xhr) {\
    \   return function () {\
    \       xhr.open(config.method,\
    \                config.url,\
    \                config.async,\
    \                config.user,\
    \                config.password);\
    \   }\
    \}" :: forall r. Fn2 OpenConfig XHR (EffAjax r Unit)

open :: forall r. OpenConfig -> XHR -> EffAjax r Unit
open = runFn2 openImpl

foreign import send "\
    \function send (xhr) {\
    \   return function() {\
    \       xhr.send();\
    \   }\
    \}" :: forall r. XHR -> EffAjax r Unit

foreign import sendWithBodyImpl "\
    \function sendWithBodyImpl (body, xhr) {\
    \   return function() {\
    \       xhr.send(body);\
    \   }\
    \}" :: forall a r. Fn2 a XHR (EffAjax r Unit)

sendWithBody :: forall a r. a -> XHR -> EffAjax r Unit
sendWithBody = runFn2 sendWithBodyImpl

foreign import getAllResponseHeaders "\
    \function getAllResponseHeaders (xhr) {\
    \   return function() {\
    \       return xhr.getAllResponseHeaders();\
    \   }\
    \}" :: forall r. XHR -> EffAjax r String

foreign import getResponseHeaderImpl "\
    \function getResponseHeaderImpl (header, xhr) {\
    \   return function() {\
    \       return xhr.getResponseHeader(header);\
    \   }\
    \}" :: forall r. Fn2 String XHR (EffAjax r String)

getResponseHeader :: forall r. String -> XHR -> EffAjax r String
getResponseHeader = runFn2 getResponseHeaderImpl

foreign import getterImpl "\
    \function getterImpl (prop, xhr) {\
    \   return function () {\
    \       return xhr[prop];\
    \   }\
    \}" :: forall r b. Fn2 String XHR (EffAjax r b)

getReadyState' :: forall r. XHR -> EffAjax r Number
getReadyState' = runFn2 getterImpl "readyState"

getReadyState :: forall r. XHR -> EffAjax r ReadyState
getReadyState xhr = parseReadyState <$> getReadyState' xhr

getResponseText :: forall r. XHR -> EffAjax r String
getResponseText = runFn2 getterImpl "responseText"

type MaybeCtors a =
    { just    :: a -> Maybe a
    , nothing :: Maybe a
    }

foreign import getResponseXMLImpl "\
    \function getResponseXMLImpl (ctor, xhr) {\
    \   return function () {\
    \       if (xhr.responseXML) {\
    \           return ctor.just(xhr.responseXML);\
    \       } else {\
    \           return ctor.nothing;\
    \       }\
    \   }\
    \}" :: forall r.  Fn2 (MaybeCtors String) XHR (EffAjax r (Maybe String))

getResponseXML :: forall r. XHR -> EffAjax r (Maybe String)
getResponseXML = runFn2 getResponseXMLImpl {just: Just, nothing: Nothing}

getStatus :: forall r. XHR -> EffAjax r Number
getStatus = runFn2 getterImpl "status"

getStatusText :: forall r. XHR -> EffAjax r String
getStatusText = runFn2 getterImpl "statusText"

foreign import overrideMimeTypeImpl "\
    \function overrideMimeTypeImpl (mime, xhr) {\
    \   return function() {\
    \       xhr.overrideMimeType(mime);\
    \   }\
    \}" :: forall r. Fn2 String XHR (EffAjax r Unit)

overrideMimeType :: forall r. String -> XHR -> EffAjax r Unit
overrideMimeType = runFn2 overrideMimeTypeImpl

foreign import setRequestHeaderImpl "\
    \function setRequestHeaderImpl (header, value, xhr) {\
    \   return function() {\
    \       xhr.setRequestHeader(header, value);\
    \   }\
    \}" :: forall r. Fn3 String String XHR (EffAjax r Unit)

setRequestHeader :: forall r. String -> String -> XHR -> EffAjax r Unit
setRequestHeader = runFn3 setRequestHeaderImpl

foreign import setterImpl "\
    \function setterImpl (prop, v, xhr) {\
    \   return function () {\
    \       xhr[prop] = v;\
    \   }\
    \}" :: forall r a. Fn3 String a XHR (EffAjax r Unit)

setOnReadyStateChange :: forall r. EffAjax r Unit -> XHR -> EffAjax r Unit
setOnReadyStateChange = runFn3 setterImpl "onreadystatechange"

setOnAbort :: forall r. EffAjax r Unit -> XHR -> EffAjax r Unit
setOnAbort = runFn3 setterImpl "onabort"

setOnError :: forall r. EffAjax r Unit -> XHR -> EffAjax r Unit
setOnError = runFn3 setterImpl "onerror"

setOnLoad :: forall r. EffAjax r Unit -> XHR -> EffAjax r Unit
setOnLoad = runFn3 setterImpl "onload"

setOnLoadEnd :: forall r. EffAjax r Unit -> XHR -> EffAjax r Unit
setOnLoadEnd = runFn3 setterImpl "onloadend"

setOnProgress :: forall r. EffAjax r Unit -> XHR -> EffAjax r Unit
setOnProgress = runFn3 setterImpl "onprogress"

setOnTimeout :: forall r. EffAjax r Unit -> XHR -> EffAjax r Unit
setOnTimeout = runFn3 setterImpl "ontimeout"

setTimeout :: forall r. Number -> XHR -> EffAjax r Unit
setTimeout = runFn3 setterImpl "timeout"

setWithCredentials :: forall r. Boolean -> XHR -> EffAjax r Unit
setWithCredentials = runFn3 setterImpl "withCredentials"

foreign import encodeUrlParams "\
    \function encodeUrlParams (obj) {\
    \   var str = '';\
    \   for (var key in obj) {\
    \       str += key + '=' + encodeURIComponent(obj[key]) + '&';\
    \   }\
    \   return str.slice(0,-1);\
    \}" :: forall r. {|r} -> String

foreign import encodeMultipart "\
    \function encodeMultipart (obj) {\
    \   var form = new FormData(); \
    \   for (var key in obj) {\
    \       form.append(key, obj[key]);\
    \   }\
    \   return form;\
    \}" :: forall r. {|r} -> FormData
