{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
module Pwa.GenPwa where

import Control.Monad (forM, when)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Utils
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Hamlet (Html, Render, shamletFile)
import Text.Julius (JavascriptUrl, juliusFile, rawJS, renderJavascriptUrl)
import Pwa.Config
import System.Directory (copyFile, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath.Posix ((</>), makeRelative)
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Vector as V

import Paths_ghcjs_pwa (getDataDir, getDataFileName)

-- from GHCJS build
rts_js        = "rts.js"        :: String
lib_js        = "lib.js"        :: String
out_js        = "out.js"        :: String
runmain_js    = "runmain.js"    :: String

-- we will generate these
index_html                 = "index.html"                 :: String
manifest_json              = "manifest.json"              :: String
service_worker_register_js = "service-worker-register.js" :: String
service_worker_js          = "service-worker.js"          :: String

-- TODO apple defines expected sizes
--      microsoft expects 144x144 for icon (but browserconfig.xml has replaced this?)
indexTemplate :: Config -> Html
indexTemplate config = $(shamletFile "resources/index.hamlet")

manifestTemplate :: Config -> Value
manifestTemplate config =
  let icons' = Array $ V.fromList $ (flip fmap) (icons config) $ \icon ->
                  object $ [ "src"   .=  path icon
                           , "sizes" .=  L.intercalate " " (sizes icon)
                           ] ++ catMaybes
                           [ "type"  .=? (iType icon)
                           ]
  in object $
    [ "name"             .= name config
    , "short_name"       .= shortName config
    , "icons"            .= icons'
    , "start_url"        .= index_html
    , "display"          .= show (display     config)
    , "orientation"      .= show (orientation config)
    , "dir"              .= show (dir         config)
    , "background_color" .= backgroundColour config
    ] ++ catMaybes
    [ "theme_color"      .=? themeColour config
    , "scope"            .=? (show <$> scope config)
    ]

listAllFiles :: FilePath -> IO [FilePath]
listAllFiles dir = do
  files <- listDirectory dir
  paths <- forM files $ \name -> do
    let path = dir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then listAllFiles path
      else return [path]
  return (concat paths)

renderJavascript' :: JavascriptUrl url -> LT.Text
renderJavascript' =
  renderJavascriptUrl (\_ _ -> undefined)

genPwa :: Config -> FilePath -> IO ()
genPwa config target = do

  -- check GHCJS resources are found
  exist <- forM [rts_js , lib_js, out_js, runmain_js] (doesFileExist . (target </>))
  when (any not exist) $ error $ "GHCJS generated resources not found (in " <> target <> ")"

  writeFile (target </> index_html) $ renderHtml $ indexTemplate config

  BS.writeFile (target </> manifest_json) $ encodePretty $ manifestTemplate config

  TIO.writeFile (target </> service_worker_register_js) $ renderJavascript' $(juliusFile "resources/service-worker-register.julius")

  -- cache all files (except service worker) found in target directory
  filesToCache <- listAllFiles target >>= return . (makeRelative target <$>) >>= return . filter (/= service_worker_js)

  -- TODO alternatively client may want to create serviceworker themselves (e.g. with ghcjs)
  (serviceWorkerTemplate config) config filesToCache >>= \sw ->
    TIO.writeFile (target </> service_worker_js) $ renderJavascript' sw
