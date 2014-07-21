# Module Documentation

## Module Network.XHR

### Types

    type Callback r = XMLHttpRequest -> EffXHR r Unit

    type EffXHR r = Eff (xhr :: XHR | r)

    type Method  = String

    data ReadyState where
      UNSENT :: ReadyState
      OPENED :: ReadyState
      HEADERSRECEIVED :: ReadyState
      LOADING :: ReadyState
      DONE :: ReadyState
      UNKNOWN :: Number -> ReadyState

    type RequestConfig r = { withCredentials :: Boolean, requestTimeout :: Number, onLoadEnd :: Callback r, onTimeout :: Callback r, onLoad :: Callback r, onError :: Callback r, onAbort :: Callback r, onProgress :: Callback r, onLoadStart :: Callback r, onReadyStateChange :: Callback r }

    data RequestType where
      QueryParam :: RequestType
      UrlEncoded :: RequestType
      Multipart :: RequestType

    type URI  = String

    data XHR :: !

    data XMLHttpRequest :: *


### Type Class Instances

    instance eqReadyState :: Eq ReadyState

    instance showReadyState :: Show ReadyState


### Values

    abort :: forall r. XMLHttpRequest -> EffXHR r Unit

    defaultConfig :: forall r. RequestConfig r

    delete :: forall r o. RequestConfig r -> URI -> {  | o } -> EffXHR r Unit

    get :: forall r o. RequestConfig r -> URI -> {  | o } -> EffXHR r Unit

    post :: forall r o. RequestConfig r -> URI -> {  | o } -> EffXHR r Unit

    put :: forall r o. RequestConfig r -> URI -> {  | o } -> EffXHR r Unit

    readyState :: XMLHttpRequest -> ReadyState

    request :: forall r h o. Method -> RequestType -> {  | h } -> RequestConfig r -> URI -> {  | o } -> EffXHR r Unit

    responseText :: XMLHttpRequest -> String

    responseXML :: XMLHttpRequest -> String

    status :: XMLHttpRequest -> Number

    statusText :: XMLHttpRequest -> String



