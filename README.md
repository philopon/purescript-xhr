# Module Documentation

## Module Network.XHR

### Types

    type Callback r = XMLHttpRequest -> EffXHR r Unit

    type Callbacks r = { onLoadEnd :: Callback r, onTimeout :: Callback r, onLoad :: Callback r, onError :: Callback r, onAbort :: Callback r, onProgress :: Callback r, onLoadStart :: Callback r, onReadyStateChange :: Callback r }

    type EffXHR r = Eff (xhr :: XHR | r)

    type Method  = String

    data ReadyState where
      UNSENT :: ReadyState
      OPENED :: ReadyState
      HEADERSRECEIVED :: ReadyState
      LOADING :: ReadyState
      DONE :: ReadyState
      UNKNOWN :: Number -> ReadyState

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

    defaultCallbacks :: forall r. Callbacks r

    delete :: forall r o. URI -> Callbacks r -> {  | o } -> EffXHR r Unit

    get :: forall r o. URI -> Callbacks r -> {  | o } -> EffXHR r Unit

    post :: forall r o. RequestType -> URI -> Callbacks r -> {  | o } -> EffXHR r Unit

    readyState :: XMLHttpRequest -> ReadyState

    request :: forall r o. Method -> RequestType -> URI -> Callbacks r -> {  | o } -> EffXHR r Unit

    responseText :: XMLHttpRequest -> String

    responseXML :: XMLHttpRequest -> String

    status :: XMLHttpRequest -> Number

    statusText :: XMLHttpRequest -> String



