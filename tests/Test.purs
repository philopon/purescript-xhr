module Test.Main where

  import Control.Monad.Eff
  import Network.XHR
  import qualified Network.XHR.Internal as I
  import Test.PSpec
  import Test.PSpec.Mocha
  import Test.Assert.Simple
  import Data.String
  import Data.Maybe
  import Data.Tuple
  import Debug.Trace
  import Data.Function

  foreign import xhrHas """
function xhrHas(key, xhr){
  return key in xhr;
}""" :: Fn2 String I.XHR Boolean

  commonCheck contentType status statusText res = do
    getResponseHeader "Content-Type" res >>= assertEqual contentType
    getStatus res >>= assertEqual status
    getStatusText res >>= assertEqual statusText

  main = do
    hasOnTimeout <- runFn2 xhrHas "ontimeout" <$> I.newXMLHttpRequest
    hasOnAbort   <- runFn2 xhrHas "onabort"   <$> I.newXMLHttpRequest
    runMocha $ do
      describe "Normal" $ do
        itAsync "no_param" $ \done ->
          get defaultAjaxOptions
            { onReadyStateChange = onDone $ \res -> do
                commonCheck "text/plain" 200 "OK" res
                getResponseText res >>= assertEqual "no_param"
               -- getResponseXML  res >>= assertEqual Nothing -- PENDING: Just {} (ie9)
                itIs done
            } "/api/mock" {template: "no_param"}

        itAsync "xml" $ \done -> 
          get defaultAjaxOptions
            { onReadyStateChange = onDone $ \res -> do
                commonCheck "text/xml" 200 "OK" res
                getResponseXML res >>= isJust >>> assertBool "should has XML response"
                itIs done
            } "/api/mock" {template: "<?xml version=\"1.0\"?><root/>", contentType: "text/xml"}

        itAsync "param" $ \done ->
          get defaultAjaxOptions
            { onReadyStateChange = onDone $ \res -> do
                commonCheck "text/plain" 200 "OK" res
                getResponseText res >>= assertEqual "q=foo,template={{#queryParams}}{{key}}={{value}},{{/queryParams}},"
                itIs done
            } "/api/mock" {q: "foo", template: "{{#queryParams}}{{key}}={{value}},{{/queryParams}}"}

        itAsync "urlencoded" $ \done ->
          post defaultAjaxOptions
            { onReadyStateChange = onDone $ \res -> do
                commonCheck "text/plain" 200 "OK" res
                getResponseText res >>= assertEqual "url-encoded"
                itIs done
            } "/api/mock/" {} (urlEncoded {q: "bar", template: "{{request-body-type}}"})

        itAsync "multipart" $ \done ->
          post defaultAjaxOptions
            { onReadyStateChange = onDone $ \res -> do
                commonCheck "text/plain" 200 "OK" res
                getResponseText res >>= assertIn ["multipart:baz", "url-encoded:baz"] -- url-encoded: ie9 polyfill.
                itIs done
            } "/api/mock" {} (multipart {q: "baz", template:  "{{request-body-type}}:{{reqBody.q}}" })

        it "sync" $ do
          _ <- get defaultAjaxOptions{async = false} "/api/no_param" {}
          return unit

        itAsync "rawbody" $ \done ->
          post defaultAjaxOptions
            { headers = [Tuple "Content-Type" "application/json"]
            , onReadyStateChange = onDone $ \res -> do
                commonCheck "text/plain" 200 "OK" res
                getResponseText res >>= assertEqual "unknown"
                itIs done
              } "/api/mock" {template: "{{request-body-type}}"} (rawBody "{ \"q\": \"qux\"}")

        itAsync "additional headers" $ \done ->
          get defaultAjaxOptions
            { headers = [Tuple "X-Test-Header" "quux"]
            , onReadyStateChange = onDone $ \res -> do
                commonCheck "text/plain" 200 "OK" res
                getResponseText res >>= assertEqual "X-Test-Header: quux"
                itIs done
              } "/api/mock" {template: "X-Test-Header: {{request-header.x-test-header}}"}

      describe "Abnormal" $ do
        itAsync "not found" $ \done ->
          post defaultAjaxOptions
            { onReadyStateChange = onDone $ \res -> do
                commonCheck "text/plain" 404 "Not Found" res
                getResponseText res >>= assertEqual "404 Page Notfound.\n"
                itIs done
            } "/api/mock" {status: 404, statusMessage: "Not Found", template: "404 Page Notfound.\n"} noBody

        skipUnless hasOnAbort $ itAsync "aborted" $ \done -> do
          task <- get defaultAjaxOptions
            { onAbort = \_ -> itIs done } "/api/mock" {}
          abort task

        skipUnless hasOnTimeout $ setTimeout 5000 $ itAsync "timeout" $ \done ->
             get defaultAjaxOptions
               { timeout   = 1000
               -- TODO: onreadystatechange called when timeout.

            --   , onReadyStateChange = onDone $ \res -> do
            --      getResponseText res >>= itIsNot done
               , onTimeout = \res -> itIs done
               } "/api/mock" {delay: 100}
