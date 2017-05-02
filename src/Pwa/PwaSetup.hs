{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Pwa.PwaSetup
  ( pwaUserHooks
  , postBuildHook
  ) where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Simple.Setup (BuildFlags, CopyFlags, InstallFlags)
import Pwa.GenPwa (genPwa)
import Pwa.Config
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath.Posix ((</>), takeDirectory)
import qualified Data.List as L
import qualified Data.Text as T

copy :: FilePath -> FilePath -> String -> IO ()
copy resourcesDir targetDir resource = do
  let src  = resourcesDir </> resource
  let dest = targetDir    </> resource
  createDirectoryIfMissing True $ takeDirectory dest
  copyFile src dest

copyIfRelative :: FilePath -> FilePath -> String -> IO ()
copyIfRelative resourcesDir targetDir resource =
  when (not (L.isPrefixOf "http://" resource) && not (L.isPrefixOf "https://" resource)) $ do
    copy resourcesDir targetDir resource

postBuildHook :: Config -> Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postBuildHook config args flags pkg lbi = do
  putStrLn "in postBuildHook"
  putStrLn $ "buildDir: " <> buildDir lbi
  putStrLn $ "installDirTemplates: " <> show (installDirTemplates lbi)
  -- putStrLn $ "localPkgDescr" <> show (localPkgDescr lbi)
  let exeName' = case executables $ localPkgDescr lbi of
                   executable : _ -> exeName executable
                   _              -> error "No executable"
  putStrLn $ "exeName: " <> exeName'

  let resourceDir' = resourceDir $ build config
      targetDir' = buildDir lbi </> exeName' </> exeName' <> ".jsexe"

  putStrLn $ "targetDir: " <> targetDir'

  --createDirectoryIfMissing True targetDir'

  copyIfRelative resourceDir' targetDir' $ favicon config
  traverse_ (copyIfRelative resourceDir' targetDir' . path) $ icons config

  genPwa config targetDir'

preCopyHook :: Args -> CopyFlags -> IO HookedBuildInfo
preCopyHook _ _ = do
  putStrLn "in preCopyHook"
  return emptyHookedBuildInfo

postCopyHook :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postCopyHook _ _ _ _ = do
  putStrLn "in postCopyHook"

pwaUserHooks :: Config -> UserHooks
pwaUserHooks config = simpleUserHooks { postBuild = postBuildHook config
                                      , preCopy   = preCopyHook
                                      , postCopy  = postCopyHook
                                      }
