module Network.XHR
    ( XHR(..)
    , EffXHR(..)
    , URI(..)

    , XMLHttpRequest(..)
    , ReadyState(..)

    , Callback(..)

    , Method(..)
    , RequestType(..)

    , RequestConfig(..)
    , defaultConfig

    , status, statusText
    , readyState
    , responseText, responseXML

    , abort

    , request
    , get, post, delete, put
    ) where

import Control.Monad.Eff

type Method = String

data ReadyState
    = UNSENT
    | OPENED
    | HEADERSRECEIVED
    | LOADING
    | DONE
    | UNKNOWN Number

instance showReadyState :: Show ReadyState where
    show UNSENT          = "UNSENT"
    show OPENED          = "OPENED"
    show HEADERSRECEIVED = "HEADERS_RECEIVED"
    show LOADING         = "LOADING"
    show DONE            = "DONE"
    show (UNKNOWN i)     = "UNKNOWN " ++ show i

instance eqReadyState :: Eq ReadyState where
    (==) UNSENT          UNSENT          = true
    (==) OPENED          OPENED          = true
    (==) HEADERSRECEIVED HEADERSRECEIVED = true
    (==) LOADING         LOADING         = true
    (==) DONE            DONE            = true
    (==) (UNKNOWN i)     (UNKNOWN j)     = i == j
    (==) _ _ = false
    (/=) a b = not (a == b)

numberToReadyState :: Number -> ReadyState
numberToReadyState i = case i of
    0 -> UNSENT
    1 -> OPENED
    2 -> HEADERSRECEIVED
    3 -> LOADING
    4 -> DONE
    i -> UNKNOWN i

type URI = String

foreign import data XMLHttpRequest :: *
foreign import data XHR :: !
type EffXHR r = Eff (xhr :: XHR | r)

type Callback r = XMLHttpRequest -> EffXHR r Unit

foreign import newXMLHttpRequest
    "function newXMLHttpRequest () {\
    \   return new XMLHttpRequest();\
    \}" :: forall r. EffXHR r XMLHttpRequest

foreign import assignCallback
    "function assignCallback (field) {\
    \   return function (xhr) {\
    \       return function (callback) {\
    \           return function () {\
    \               if (callback) {\
    \                   xhr[field] = function(){callback(xhr)()}\
    \}}}}}" :: forall r. String -> XMLHttpRequest -> Callback r -> EffXHR r Unit

foreign import open
    "function open (method) {\
    \   return function(uri) {\
    \       return function(xhr) {\
    \           return function() {\
    \               xhr.open(method, uri);\
    \}}}}" :: forall r. String -> URI -> XMLHttpRequest -> EffXHR r Unit

foreign import send
    "function send (body) {\
    \   return function (xhr) {\
    \       return function () {\
    \           return xhr.send(body);\
    \}}}" :: forall body r. body -> XMLHttpRequest -> EffXHR r Unit

foreign import abort
    "function abort (xhr) {\
    \   return function () {\
    \       xhr.abort();\
    \}}" :: forall r. XMLHttpRequest -> EffXHR r Unit

foreign import jsNull "var jsNull = null;" :: forall b. b

foreign import unsafeGetter
    "function unsafeGetter (key) {\
    \   return function (obj) {\
    \       return obj[key];\
    \}}" :: forall object key value. key -> object -> value

foreign import unsafeSetter
    "function unsafeSetter (key) {\
    \   return function(value) {\
    \       return function(obj) {\
    \           return function() {\
    \               obj[key] = value;\
    \}}}}" :: forall r value object. String -> value -> object -> Eff r Unit

foreign import getResponseHeader
    "function getResponseHeader (header) {\
    \   return function (xhr) {\
    \       return function () {\
    \           return xhr.getResponseHeader(header);\
    \}}}" :: forall r. String -> XMLHttpRequest -> EffXHR r String

foreign import setRequestHeaders
    "function setRequestHeaders (obj) {\
    \       return function (xhr) {\
    \           return function() {\
    \               for(var key in obj) {\
    \                   xhr.setRequestHeader(key, obj[key]);\
    \}}}}" :: forall o r. { |o} -> XMLHttpRequest -> EffXHR r Unit

foreign import objectToString
    "function objectToString (obj) {\
    \   var str = '';\
    \   for (var key in obj) {\
    \       str += key + '=' + encodeURIComponent(obj[key]) + '&'\
    \   }\
    \   return str.slice(0,-1);\
    \}" :: forall r. { | r} -> String

foreign import data FormData :: *

foreign import newFormData
    "function newFormData () {\
    \   return new FormData();\
    \}" :: forall r. EffXHR r FormData

foreign import appendForm
    "function appendForm (obj) {\
    \       return function (form) {\
    \           return function () {\
    \               for (var key in obj) {\
    \                   form.append(key, obj[key]);\
    \}}}}" :: forall o r. {|o} -> FormData -> EffXHR r Unit

status :: XMLHttpRequest -> Number
status o = unsafeGetter "status" o

statusText :: XMLHttpRequest -> String
statusText o = unsafeGetter "statusText" o

readyState :: XMLHttpRequest -> ReadyState
readyState o = numberToReadyState $ unsafeGetter "readyState" o

responseText :: XMLHttpRequest -> String
responseText o = unsafeGetter "responseText" o

responseXML :: XMLHttpRequest -> String
responseXML o = unsafeGetter "responseXML" o

preProcess :: forall r. Method -> URI
           -> RequestConfig r -> EffXHR r XMLHttpRequest
preProcess method uri cfg = do
    r <- newXMLHttpRequest
    assignCallback "onreadystatechange" r cfg.onReadyStateChange
    assignCallback "onloadstart" r cfg.onLoadStart
    assignCallback "onprogress" r cfg.onProgress
    assignCallback "onabort" r cfg.onAbort
    assignCallback "onerror" r cfg.onError
    assignCallback "onload" r cfg.onLoad
    assignCallback "ontimeout" r cfg.onTimeout
    assignCallback "onloadend" r cfg.onLoadEnd
    unsafeSetter "timeout" cfg.requestTimeout r
    unsafeSetter "withCredentials" cfg.withCredentials r
    open method uri r
    return r

data RequestType
    = QueryParam
    | UrlEncoded
    | Multipart

request :: forall r h o. Method -> RequestType -> {|h} -> RequestConfig r -> URI
        -> {|o} -> EffXHR r Unit
request method typ h cfg uri prm = case typ of
    QueryParam -> do
        let p = objectToString prm
            p' = if p == "" then "" else "?" ++ p
        r <- preProcess method (uri ++ p') cfg
        setRequestHeaders h r
        send jsNull r

    UrlEncoded -> do
        let b = objectToString prm
        r <- preProcess method uri cfg
        setRequestHeaders h r
        setRequestHeaders {"Content-Type": "application/x-www-form-urlencoded"} r
        send b r
    Multipart -> do
        f <- newFormData
        appendForm prm f
        r <- preProcess method uri cfg
        setRequestHeaders h r
        send f r

get :: forall r o. RequestConfig r -> URI -> { |o} -> EffXHR r Unit
get = request "GET" QueryParam {}

post :: forall r o. RequestConfig r -> URI -> { |o} -> EffXHR r Unit
post = request "POST" UrlEncoded {}

delete :: forall r o. RequestConfig r -> URI -> { |o} -> EffXHR r Unit
delete = request "DELETE" QueryParam {}

put :: forall r o. RequestConfig r -> URI -> { |o} -> EffXHR r Unit
put = request "PUT" UrlEncoded {}

type RequestConfig r =
    { onReadyStateChange :: Callback r
    , onLoadStart        :: Callback r
    , onProgress         :: Callback r
    , onAbort            :: Callback r
    , onError            :: Callback r
    , onLoad             :: Callback r
    , onTimeout          :: Callback r
    , onLoadEnd          :: Callback r

    , requestTimeout     :: Number
    , withCredentials    :: Boolean
    }

defaultConfig :: forall r. RequestConfig r
defaultConfig =
    { onReadyStateChange: jsNull
    , onLoadStart       : jsNull
    , onProgress        : jsNull
    , onAbort           : jsNull
    , onError           : jsNull
    , onLoad            : jsNull
    , onTimeout         : jsNull
    , onLoadEnd         : jsNull
    
    , requestTimeout    : 0
    , withCredentials   : false
    }
