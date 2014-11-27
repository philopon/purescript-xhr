# Module Documentation

## Module Network.XHR

### Types

    type AjaxOptions r = { onTimeout :: Callback r, onReadyStateChange :: Callback r, onProgress :: Callback r, onLoadEnd :: Callback r, onLoad :: Callback r, onError :: Callback r, onAbort :: Callback r, password :: String, user :: String, async :: Boolean, credentials :: Boolean, timeout :: Number, cache :: Boolean, headers :: [Tuple String String], url :: String, method :: String }

    type Callback r = Response -> EffAjax r Unit

    type EffAjax r = Eff (ajax :: I.Ajax | r)

    type Query a = {  | a }

    newtype Response

    type URL = String

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

    multipart :: forall a. {  | a } -> Body I.FormData

    noBody :: forall a. Body a

    onDone :: forall r. Callback r -> Callback r

    onHeaderReceived :: forall r. Callback r -> Callback r

    onLoading :: forall r. Callback r -> Callback r

    onOpened :: forall r. Callback r -> Callback r

    onSuccess :: forall r. Callback r -> Callback r

    onUnsent :: forall r. Callback r -> Callback r

    post :: forall r a b. AjaxOptions r -> URL -> Query a -> Body b -> EffAjax r XHRTask

    rawBody :: String -> Body String

    unsafeToResponse :: XHRTask -> Response

    urlEncoded :: forall a. {  | a } -> Body String


## Module Network.XHR.Types

### Types

    data Body a where
      NoBody :: Body a
      RawBody :: a -> Body a
      UrlEncoded :: a -> Body a
      Multipart :: a -> Body a

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