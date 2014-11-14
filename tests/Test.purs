module Main where

import Control.Monad.Eff
import Network.XHR
import qualified Network.XHR.Internal as I
import Test.Mocha (it, itAsync, itSkip, DoneToken(..), Done(..), describe)
import Test.Chai
import Data.Maybe
import Data.Tuple

foreign import itIs
  "function itIs(d){ return function(){d()}; }" :: forall eff. 
                                       DoneToken -> 
                                       Eff (done :: Done | eff) Unit

foreign import isPhantom
    "var isPhantom = !!window.mochaPhantomJS;" :: Boolean

checkLoadEnd mbbdy ct st stt res = do
    case mbbdy of
        Nothing -> return unit
        Just b  -> getResponseText res >>= toEqual (expect b)
    getResponseHeader "Content-Type" res >>= toEqual (expect ct)
    getStatus res >>= toEqual (expect st)
    getStatusText res >>= toEqual (expect stt)

main = do
    describe "Normal" $ do
        itAsync "no_param" $ \done ->
            get defaultAjaxOptions
            { onReadyStateChange = onDone $ \res -> do
                checkLoadEnd (Just "no_param") "text/plain; charset=utf-8" 200 "OK" res
                itIs done
            } "/api/no_param" {}

        itAsync "xml" $ \done -> 
            get defaultAjaxOptions
            { onReadyStateChange = onDone $ \res -> do
                checkLoadEnd Nothing "application/xml" 200 "OK" res

                getResponseXML res >>= toNotDeepEqual (expect (Just 1))
                itIs done
            } "/test.xml" {}

        itAsync "param" $ \done ->
            get defaultAjaxOptions
            { onReadyStateChange = onDone $ \res -> do
                checkLoadEnd (Just "q param: foo") "text/html; charset=utf-8" 200 "OK" res
                itIs done
            } "/api/param" {q: "foo"}

        itAsync "urlencoded" $ \done ->
            post defaultAjaxOptions
            { onReadyStateChange = onDone $ \res -> do
                checkLoadEnd (Just "q body: foo") "text/html; charset=utf-8" 200 "OK" res
                itIs done
            } "/api/body" {} (urlEncoded {q: "foo"})

        itAsync "multipart" $ \done ->
            post defaultAjaxOptions
            { onReadyStateChange = onDone $ \res -> do
                checkLoadEnd (Just "q body: foo") "text/html; charset=utf-8" 200 "OK" res
                itIs done
            } "/api/body" {} (multipart {q: "foo"})

        it "sync" $ do
            task <- get defaultAjaxOptions{async = false} "/api/no_param" {}
            return unit

        itAsync "rawbody" $ \done ->
            post defaultAjaxOptions
            { headers = [Tuple "Content-Type" "application/json"]
            , onReadyStateChange = onDone $ \res -> do
                checkLoadEnd (Just "q body: foo") "text/html; charset=utf-8" 200 "OK" res
                itIs done
            } "/api/body" {} (rawBody "{ \"q\": \"foo\"}")

        itAsync "additional headers" $ \done ->
            get defaultAjaxOptions
            { headers = [Tuple "X-Test-Header" "foo"]
            , onReadyStateChange = onDone $ \res -> do
                checkLoadEnd (Just "X-Test-Header: foo") "text/html; charset=utf-8" 200 "OK" res
                itIs done
            } "/api/headers" {}

    describe "Abnormal" $ do
        itAsync "not found" $ \done ->
            post defaultAjaxOptions
            { onReadyStateChange = onDone $ \res -> do
                checkLoadEnd (Just "Cannot POST /no/route\n") "text/html; charset=utf-8" 404 "Not Found" res
                itIs done
            } "/no/route" {} noBody

        itAsync "aborted" $ \done -> do
            task <- get defaultAjaxOptions
                { onAbort = \_ -> itIs done } "/api/no_param" {}
            abort task

        if isPhantom
            then itSkip "timeout" (return unit)
            else itAsync "timeout" $ \done ->
                    get defaultAjaxOptions
                    { timeout   = 1
                    , onTimeout = \res -> itIs done
                    } "/api/no_param" {}

        itAsync "no XML" $ \done ->
            get defaultAjaxOptions
            { onReadyStateChange = onDone $ \res -> do
                checkLoadEnd Nothing "text/plain; charset=utf-8" 200 "OK" res
                getResponseXML res >>= toEqual (expect Nothing)
                itIs done
            } "/api/no_param" {}
