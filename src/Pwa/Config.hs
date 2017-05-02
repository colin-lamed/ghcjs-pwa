{-# LANGUAGE TemplateHaskell   #-}

module Pwa.Config where

import Text.Julius (JavascriptUrl, juliusFile, rawJS)
import Data.Semigroup ((<>))
import Control.Monad (liftM)
import System.Random (randomRs, newStdGen)


data Dir = LTR | RTL | Auto

instance Show Dir where
  show LTR  = "ltr"
  show RTL  = "rtl"
  show Auto = "auto"


data Display = Standalone
             | Fullscreen
             | MinimalUi
             | Browser

instance Show Display where
  show Standalone = "standalone"
  show Fullscreen = "fullscreen"
  show MinimalUi  = "minimal-ui"
  show Browser    = "browser"

data Orientation = Any
                 | Natural
                 | Landscape
                 | LandscapePrimary
                 | LandscapeSecondary
                 | Portrait
                 | PortraitPrimary
                 | PortraitSecondary

instance Show Orientation where
  show Any                = "any"
  show Natural            = "natural"
  show Landscape          = "landscape"
  show LandscapePrimary   = "landscape-primary"
  show LandscapeSecondary = "landscape-secondary"
  show Portrait           = "portrait"
  show PortraitPrimary    = "portrait-primary"
  show PortraitSecondary  = "portrait-secondary"


data Build = Build
  { resourceDir :: String
  }

type Colour = String

data Icon = Icon
  { path   :: String
  , sizes  :: [String]
  , iType  :: Maybe String
  }

type ServiceWorkerTemplate = Config -> [String] -> IO (JavascriptUrl ())

data Config = Config
  { name                  :: String
  , shortName             :: String
  , description           :: Maybe String
  , dir                   :: Dir
  , display               :: Display
  , orientation           :: Orientation
  , tileColour            :: Colour
  , backgroundColour      :: Colour
  , themeColour           :: Maybe Colour
  , favicon               :: String
  , icons                 :: [Icon]
  , scope                 :: Maybe String
  , serviceWorkerTemplate :: ServiceWorkerTemplate
  , build                 :: Build
  }

-- | ServiceWorkerTemplate to enable offline usage.
-- It caches all resources, and will serve resources from cache.
-- Cache name has a random suffix so should be unique for each build.
offlineServiceWorkerTemplate :: ServiceWorkerTemplate
offlineServiceWorkerTemplate config filesToCache = do
  rndString <- liftM (take 10 . randomRs ('a','z')) newStdGen
  let cacheName :: String
      cacheName = shortName config <> "-" <> rndString
  return $(juliusFile "resources/offline-service-worker.julius")


config = Config
  { name                  = "GHCJS PWA"
  , shortName             = "GHCJS PWA"
  , description           = Nothing
  , dir                   = Auto
  , display               = Browser
  , orientation           = PortraitPrimary
  , tileColour            = "#FFFFFF"
  , backgroundColour      = "#FFFFFF"
  , themeColour           = Nothing
  , favicon               = "icons/favicon.jpg"
  , icons                 = []
  , scope                 = Nothing
  , serviceWorkerTemplate = offlineServiceWorkerTemplate
  , build                 = Build { resourceDir = "resources" }
  }
