module Network.XHR
    ( XHR(..)
    , EffXHR(..)
    , ReadyState(..)
    , URI(..)
    , XMLHttpRequest(..)
    , Callback(..)
    , Callbacks(..)
    , RequestType(..)
    , Method(..)

    , status, statusText
    , readyState
    , responseText, responseXML
    , get, post, delete
    , request
    , defaultCallbacks
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

foreign import jsNull "var jsNull = null;" :: forall b. b

foreign import unsafeGetter
    "function unsafeGetter (key) {\
    \   return function (obj) {\
    \       return obj[key];\
    \}}" :: forall object key value. key -> object -> value

foreign import getResponseHeader
    "function getResponseHeader (header) {\
    \   return function (xhr) {\
    \       return function () {\
    \           return xhr.getResponseHeader(header);\
    \}}}" :: forall r. String -> XMLHttpRequest -> EffXHR r String

foreign import setRequestHeader
    "function setRequestHeader (key) {\
    \   return function (value) {\
    \       return function (xhr) {\
    \           return function() {\
    \               xhr.setRequestHeader(key, value);\
    \}}}}" :: forall r. String -> String -> XMLHttpRequest -> EffXHR r Unit

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
           -> Callbacks r -> EffXHR r XMLHttpRequest
preProcess method uri cb = do
    r <- newXMLHttpRequest
    assignCallback "onreadystatechange" r cb.onReadyStateChange
    assignCallback "onloadstart" r cb.onLoadStart
    assignCallback "onprogress" r cb.onProgress
    assignCallback "onabort" r cb.onAbort
    assignCallback "onerror" r cb.onError
    assignCallback "onload" r cb.onLoad
    assignCallback "ontimeout" r cb.onTimeout
    assignCallback "onloadend" r cb.onLoadEnd
    open method uri r
    return r

request :: forall r o. Method -> RequestType -> URI
        -> Callbacks r -> { | o} -> EffXHR r Unit
request method QueryParam uri cb prm = do
    let p = objectToString prm
        p' = if p == "" then "" else "?" ++ p
    r <- preProcess method (uri ++ p') cb
    send jsNull r
request method UrlEncoded uri cb bdy = do
    let b = objectToString bdy
    r <- preProcess method uri cb
    setRequestHeader "Content-Type" "application/x-www-form-urlencoded" r
    send b r
request method Multipart uri cb bdy = do
    f <- newFormData
    appendForm bdy f
    r <- preProcess method uri cb
    send f r

get :: forall r o. URI -> Callbacks r -> { |o} -> EffXHR r Unit
get = request "GET" QueryParam

post :: forall r o. RequestType -> URI -> Callbacks r -> { |o} -> EffXHR r Unit
post = request "POST"

delete :: forall r o. URI -> Callbacks r -> { |o} -> EffXHR r Unit
delete = request "DELETE" QueryParam

data RequestType
    = QueryParam
    | UrlEncoded
    | Multipart

type Callbacks r =
    { onReadyStateChange :: Callback r
    , onLoadStart        :: Callback r
    , onProgress         :: Callback r
    , onAbort            :: Callback r
    , onError            :: Callback r
    , onLoad             :: Callback r
    , onTimeout          :: Callback r
    , onLoadEnd          :: Callback r
    }

defaultCallbacks :: forall r. Callbacks r
defaultCallbacks =
    { onReadyStateChange: jsNull
    , onLoadStart       : jsNull
    , onProgress        : jsNull
    , onAbort           : jsNull
    , onError           : jsNull
    , onLoad            : jsNull
    , onTimeout         : jsNull
    , onLoadEnd         : jsNull
    }

{-
main = do
    let cbs = defaultCallbacks { onLoadEnd = \x -> print (responseText x) }
    post Multipart "/api/12" cbs {test: 24}
    -}
