# Module Documentation

## Module Network.XHR

### Types

    type AjaxOptions r = { onTimeout :: Response -> I.EffAjax r Unit, onReadyStateChange :: I.ReadyState -> Response -> I.EffAjax r Unit, onProgress :: Response -> I.EffAjax r Unit, onLoadEnd :: Response -> I.EffAjax r Unit, onLoad :: Response -> I.EffAjax r Unit, onError :: Response -> I.EffAjax r Unit, onAbort :: Response -> I.EffAjax r Unit, password :: String, user :: String, async :: Boolean, credentials :: Boolean, timeout :: Number, cache :: Boolean, headers :: [Tuple String String], url :: String, method :: String }

    data Body r where
      NoBody :: Body r
      UrlEncoded :: {  | r } -> Body r
      Multipart :: {  | r } -> Body r

    newtype Response where
      Response :: I.XHR -> Response


### Values

    ajax :: forall r a b. AjaxOptions r -> {  | a } -> Body b -> I.EffAjax r Unit

    defaultAjaxOptions :: forall r. AjaxOptions r

    getAllResponseHeaders :: forall r. Response -> I.EffAjax r String

    getReadyState :: forall r. Response -> I.EffAjax r I.ReadyState

    getResponseHeader :: forall r. String -> Response -> I.EffAjax r String

    getResponseText :: forall r. Response -> I.EffAjax r String

    getResponseXML :: forall r. Response -> I.EffAjax r String

    getStatus :: forall r. Response -> I.EffAjax r Number

    getStatusText :: forall r. Response -> I.EffAjax r String


## Module Network.XHR.Internal

### Types

    data Ajax :: !

    type EffAjax r = Eff (ajax :: Ajax | r)

    data FormData :: *

    type OpenConfig  = { password :: String, user :: String, async :: Boolean, url :: String, method :: String }

    data ReadyState where
      UNSENT :: ReadyState
      OPENED :: ReadyState
      HEADERSRECEIVED :: ReadyState
      LOADING :: ReadyState
      DONE :: ReadyState
      UNKNOWN :: Number -> ReadyState

    data XHR :: *


### Values

    abort :: forall r. XHR -> EffAjax r Unit

    defaultOpenConfig :: OpenConfig

    encodeMultipart :: forall r. {  | r } -> FormData

    encodeUrlParams :: forall r. {  | r } -> String

    getAllResponseHeaders :: forall r. XHR -> EffAjax r String

    getReadyState :: forall r. XHR -> EffAjax r ReadyState

    getResponseHeader :: forall r. String -> XHR -> EffAjax r String

    getResponseText :: forall r. XHR -> EffAjax r String

    getResponseXML :: forall r. XHR -> EffAjax r String

    getStatus :: forall r. XHR -> EffAjax r Number

    getStatusText :: forall r. XHR -> EffAjax r String

    newXMLHttpRequest :: forall r. EffAjax r XHR

    open :: forall r. OpenConfig -> XHR -> EffAjax r Unit

    overrideMimeType :: forall r. String -> XHR -> EffAjax r Unit

    send :: forall r. XHR -> EffAjax r Unit

    sendWithBody :: forall a r. a -> XHR -> EffAjax r Unit

    setOnAbort :: forall r. EffAjax r Unit -> XHR -> EffAjax r Unit

    setOnError :: forall r. EffAjax r Unit -> XHR -> EffAjax r Unit

    setOnLoad :: forall r. EffAjax r Unit -> XHR -> EffAjax r Unit

    setOnLoadEnd :: forall r. EffAjax r Unit -> XHR -> EffAjax r Unit

    setOnProgress :: forall r. EffAjax r Unit -> XHR -> EffAjax r Unit

    setOnReadyStateChange :: forall r. EffAjax r Unit -> XHR -> EffAjax r Unit

    setOnTimeout :: forall r. EffAjax r Unit -> XHR -> EffAjax r Unit

    setRequestHeader :: forall r. String -> String -> XHR -> EffAjax r Unit

    setTimeout :: forall r. Number -> XHR -> EffAjax r Unit

    setWithCredentials :: forall r. Boolean -> XHR -> EffAjax r Unit