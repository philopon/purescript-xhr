module Main where

import Control.Monad.Eff
import Network.XHR
import Test.Mocha
import Test.Chai

checkLoadEnd bdy ct st stt res = do
    getResponseText res >>= toEqual (expect bdy)
    getResponseHeader "Content-Type" res >>= toEqual (expect ct)
    getStatus res >>= toEqual (expect st)
    getStatusText res >>= toEqual (expect stt)


main = do
    itAsync "no_param" $ \done ->
        ajax defaultAjaxOptions {
        url = "/api/no_param",
        method = "GET",
        onLoadEnd = \res -> do
            checkLoadEnd "no_param" "text/plain; charset=utf-8" 200 "OK" res
            itIs done
        } {} NoBody

    itAsync "param" $ \done ->
        ajax defaultAjaxOptions {
        url = "/api/param",
        method = "GET",
        onLoadEnd = \res -> do
            checkLoadEnd "q param: foo" "text/html; charset=utf-8" 200 "OK" res
            itIs done
        } {q: "foo"} NoBody

    itAsync "urlencoded" $ \done ->
        ajax defaultAjaxOptions {
        url = "/api/body",
        method = "POST",
        onLoadEnd = \res -> do
            checkLoadEnd "q body: foo" "text/html; charset=utf-8" 200 "OK" res
            itIs done
        } {} (UrlEncoded {q: "foo"})

    itAsync "multipart" $ \done ->
        ajax defaultAjaxOptions {
        url = "/api/body",
        method = "POST",
        onLoadEnd = \res -> do
            checkLoadEnd "q body: foo" "text/html; charset=utf-8" 200 "OK" res
            itIs done
        } {} (Multipart {q: "foo"})
