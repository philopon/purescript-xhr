module Network.XHR.ReadyState where

data ReadyState
    = UNSENT
    | OPENED
    | HEADERSRECEIVED
    | LOADING
    | DONE
    | UNKNOWN Number

instance eqReadyState :: Eq ReadyState where
    (==) UNSENT UNSENT = true
    (==) OPENED OPENED = true
    (==) HEADERSRECEIVED HEADERSRECEIVED = true
    (==) LOADING LOADING = true
    (==) DONE DONE = true
    (==) (UNKNOWN a) (UNKNOWN b) = a == b
    (==) _ _ = false
    (/=) a b = not (a == b)

parseReadyState :: Number -> ReadyState
parseReadyState i = case i of
    0 -> UNSENT
    1 -> OPENED
    2 -> HEADERSRECEIVED
    3 -> LOADING
    4 -> DONE
    i -> UNKNOWN i
