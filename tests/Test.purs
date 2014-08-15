module Main where

import Control.Monad.Eff
import Network.XHR
import qualified Network.XHR.Internal as I
import Test.Mocha (itAsync, DoneToken(..), Done(..))
import Test.Chai

foreign import itIs
  "function itIs(d){ return function(){d()}; }" :: forall eff. 
                                       DoneToken -> 
                                       Eff (done :: Done | eff) Unit

checkLoadEnd bdy ct st stt res = do
    getResponseText res >>= toEqual (expect bdy)
    getResponseHeader "Content-Type" res >>= toEqual (expect ct)
    getStatus res >>= toEqual (expect st)
    getStatusText res >>= toEqual (expect stt)

main = do
    itAsync "no_param" $ \done ->
        get defaultAjaxOptions
        { onReadyStateChange = onDone $ \res -> do
            checkLoadEnd "no_param" "text/plain; charset=utf-8" 200 "OK" res
            itIs done
        } "/api/no_param" {}

    itAsync "param" $ \done ->
        get defaultAjaxOptions
        { onReadyStateChange = onDone $ \res -> do
            checkLoadEnd "q param: foo" "text/html; charset=utf-8" 200 "OK" res
            itIs done
        } "/api/param" {q: "foo"}

    itAsync "urlencoded" $ \done ->
        post defaultAjaxOptions
        { onReadyStateChange = onDone $ \res -> do
            checkLoadEnd "q body: foo" "text/html; charset=utf-8" 200 "OK" res
            itIs done
        } "/api/body" {} (UrlEncoded {q: "foo"})

    itAsync "multipart" $ \done ->
        post defaultAjaxOptions
        { onReadyStateChange = onDone $ \res -> do
            checkLoadEnd "q body: foo" "text/html; charset=utf-8" 200 "OK" res
            itIs done
        } "/api/body" {} (Multipart {q: "foo"})
