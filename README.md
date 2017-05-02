# GHCSJS-PWA

## Convert GHCJS app into a Progressive-Web-App.

During the build, the manifest.json, service worker resources and index.html will be
generated for the GHCJS app.

The service worker generated will cache all the configured and generated resources,
allowing the app to be run off-line. Each build will have a unique cache name, so
the browser cache should be cleared and reloaded for a new build.

## To use

* Configure cabal for a custom build with ghcjs-pwa as dependency:

```
build-type:          Custom
cabal-version:       >= 1.24

custom-setup
  setup-depends:   base >= 4.6
                 , Cabal >= 1.24 && < 1.25
                 , ghcjs-pwa
```

* Add a Setup.hs file to your GHCJS project.


```haskell
import Distribution.Simple (defaultMainWithHooks)
import Pwa.Config
import Pwa.PwaSetup (pwaUserHooks)

main = defaultMainWithHooks $ pwaUserHooks $ config
  { name             = "My first PWA"
  , shortName        = "My first PWA"
  , display          = Standalone
  , orientation      = PortraitPrimary
  , tileColour       = "#E09D5A"
  , backgroundColour = "#FFFFFF"
  , themeColour      = Just "#E09D5A"
  , favicon          = "icons/favicon.jpg"
  , icons            = [ Icon "icons/icon_128.png" ["128x128"] (Just "image/png")
                       , Icon "icons/icon_256.png" ["256x256"] (Just "image/png")
                       , Icon "icons/icon_512.png" ["512x512"] (Just "image/png")
                       ]
  , serviceWorkerTemplate = offlineServiceWorkerTemplate
  , build            = Build { resourceDir = "resources" }
  }
```

Where `resources` is the directory for all the references icons.
