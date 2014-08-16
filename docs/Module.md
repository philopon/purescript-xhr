# Module Documentation

## Module Network.XHR

### Types

    type AjaxOptions r = { onTimeout :: Response -> EffAjax r Unit, onReadyStateChange :: OnReadyStateChange r, onProgress :: Response -> EffAjax r Unit, onLoadEnd :: Response -> EffAjax r Unit, onLoad :: Response -> EffAjax r Unit, onError :: Response -> EffAjax r Unit, onAbort :: Response -> EffAjax r Unit, password :: String, user :: String, async :: Boolean, credentials :: Boolean, timeout :: Number, cache :: Boolean, headers :: [Tuple String String], url :: String, method :: String }

    data Body r where
      NoBody :: Body r
      UrlEncoded :: {  | r } -> Body r
      Multipart :: {  | r } -> Body r

    type EffAjax r = Eff (ajax :: I.Ajax | r)

    type OnReadyStateChange r = ReadyState -> Response -> EffAjax r Unit

    type Query a = {  | a }

    newtype Response

    type URL  = String

    newtype XHRTask


### Type Classes

    class HasReadyState a where
      getReadyState :: forall r. a -> EffAjax r ReadyState


### Type Class Instances

    instance hasReadyStateResponse :: HasReadyState Response

    instance hasReadyStateXHRTask :: HasReadyState XHRTask


### Values

    abort :: forall r. XHRTask -> EffAjax r Unit

    ajax :: forall r a b. AjaxOptions r -> Query a -> Body b -> EffAjax r XHRTask

    defaultAjaxOptions :: forall r. AjaxOptions r

    get :: forall r a. AjaxOptions r -> URL -> Query a -> EffAjax r XHRTask

    getAllResponseHeaders :: forall r. Response -> EffAjax r String

    getResponseHeader :: forall r. String -> Response -> EffAjax r String

    getResponseText :: forall r. Response -> EffAjax r String

    getResponseXML :: forall r. Response -> EffAjax r (Maybe String)

    getStatus :: forall r. Response -> EffAjax r Number

    getStatusText :: forall r. Response -> EffAjax r String

    onDone :: forall r. (Response -> EffAjax r Unit) -> OnReadyStateChange r

    onHeaderReceived :: forall r. (Response -> EffAjax r Unit) -> OnReadyStateChange r

    onLoading :: forall r. (Response -> EffAjax r Unit) -> OnReadyStateChange r

    onOpened :: forall r. (Response -> EffAjax r Unit) -> OnReadyStateChange r

    onUnsent :: forall r. (Response -> EffAjax r Unit) -> OnReadyStateChange r

    post :: forall r a b. AjaxOptions r -> URL -> Query a -> Body b -> EffAjax r XHRTask

    unsafeToResponse :: XHRTask -> Response


## Module Network.XHR.ReadyState

### Types

    data ReadyState where
      UNSENT :: ReadyState
      OPENED :: ReadyState
      HEADERSRECEIVED :: ReadyState
      LOADING :: ReadyState
      DONE :: ReadyState
      UNKNOWN :: Number -> ReadyState


### Type Class Instances

    instance eqReadyState :: Eq ReadyState


### Values

    parseReadyState :: Number -> ReadyState