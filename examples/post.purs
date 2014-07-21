module Main where

import Control.Monad.Eff
import Network.XHR
import Debug.Trace

main :: Eff (trace :: Trace, xhr :: XHR) Unit
main = do
    let cfg = defaultConfig { onLoadEnd = \x -> print (responseText x) }
    post cfg "/api/12" {test: 24}
