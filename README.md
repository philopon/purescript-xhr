# THIS PROJECT IS NO LONGER MAINTAINED

purescript-xhr
===
[![Build Status](https://travis-ci.org/philopon/purescript-xhr.svg?branch=master)](https://travis-ci.org/philopon/purescript-xhr)
[![Bower version](https://badge.fury.io/bo/purescript-xhr.svg)](http://badge.fury.io/bo/purescript-xhr)
[![devDependency Status](https://david-dm.org/philopon/purescript-xhr/dev-status.svg)](https://david-dm.org/philopon/purescript-xhr#info=devDependencies)

XMLHttpRequest binding for purescript.

- [Module documentation](docs/Network/XHR.md)

USAGE
===

GET request
---
please read [Module documentation](docs/Module.md) if you want to know about other hooks.

```.hs
getQuery = get defaultAjaxOptions
  { onReadyStateChange = onSuccess $ \response -> do
      txt <- getResponseText response
      Debug.Trace.trace txt
  } "/api/foo" {queryParam: "bar"}
```

POST request with application/x-www-form-urlencoded
---

```.hs
postQuery1 = post defaultAjaxOptions
  { onReadyStateChange = onSuccess $ \response -> do
      txt <- getResponseText response
      Debug.Trace.trace txt
  } "/api/bar" {} (urlEncoded {param: "bar"})
```

POST request with multipart/form-data
---

```.hs
postQuery2 = post defaultAjaxOptions
  { onReadyStateChange = onSuccess $ \response -> do
      txt <- getResponseText response
      Debug.Trace.trace txt
  } "/api/bar" {} (multipart {param: "bar"})
```

Abort request
---

```.hs
abortQuery = do
  task <- get defaultAjaxOptions "/api/foo" {}
  abort task
```

Sync request
---

```.hs
syncQuery = do
  task <- get defaultAjaxOptions { async = false } "/api/baz" {}
  txt  <- getResponseText (unsafeToResponsege task)
  Debug.Trace.trace txt
```

Other request
---

```.hs
delete = ajax defaultAjaxOptions
  { method = "DELETE"
  , url    = "/api/qux"
  } {} noBody
