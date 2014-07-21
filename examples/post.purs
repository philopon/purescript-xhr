module Main where

import Network.XHR
import Debug.Trace

main = do
    let cbs = defaultCallbacks { onLoadEnd = \x -> print (responseText x) }
    post Multipart "/api/12" cbs {test: 24}
