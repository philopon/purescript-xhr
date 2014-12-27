# Module Documentation

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



