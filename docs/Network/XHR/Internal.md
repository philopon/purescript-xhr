# Module Documentation

## Module Network.XHR.Internal

### Types


    data Ajax :: !


    type EffAjax r = Eff (ajax :: Ajax | r)


    data FormData :: *


    type OpenConfig = { password :: String, user :: String, async :: Boolean, url :: String, method :: String }


    data XHR :: *


    data XML :: *


### Values


    abort :: forall r. XHR -> EffAjax r Unit


    defaultOpenConfig :: OpenConfig


    encodeMultipart :: forall r. {  | r } -> FormData


    encodeUrlParams :: forall r. {  | r } -> String


    getAllResponseHeaders :: forall r. XHR -> EffAjax r String


    getReadyState :: forall r. XHR -> EffAjax r ReadyState


    getReadyState' :: forall r. XHR -> EffAjax r Number


    getResponseHeader :: forall r. String -> XHR -> EffAjax r String


    getResponseText :: forall r. XHR -> EffAjax r String


    getResponseXML :: forall r. XHR -> EffAjax r (Maybe XML)


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



