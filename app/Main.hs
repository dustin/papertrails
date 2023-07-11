{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Amazonka                     (Region (..), hashedFile, newEnv, paginate, runResourceT, send, sinkBody,
                                               toBody)
import qualified Amazonka                     as AWS
import           Amazonka.Auth                (discover)
import           Amazonka.Data.Text           (toText)
import           Amazonka.S3                  (BucketName, ObjectKey (..), newDelete, newDeleteObjects, newGetObject,
                                               newListObjectsV2, newObjectIdentifier, newPutObject)
import           Control.Lens
import           Control.Monad                (unless, void)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Conduit                 (runConduit, (.|))
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.List            as CL
import           Data.Conduit.Zlib            (ungzip)
import           Data.Generics.Labels         ()
import           Data.List                    (sort)
import           Data.Text                    (Text, drop, dropEnd, isPrefixOf, pack, splitOn, take, unpack)
import           Data.Time                    (defaultTimeLocale, formatTime, getCurrentTime)
import           Options.Applicative          (Parser, execParser, fullDesc, help, helper, info, long, progDesc,
                                               showDefault, strOption, value, (<**>))
import           System.Directory             (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive,
                                               renameFile)
import           System.Process               (callProcess)

data Options = Options {
  bucketName     :: BucketName
  , inputPrefix  :: Text
  , outputPrefix :: Text
  }

options :: Parser Options
options = Options
  <$> strOption (long "bucket" <> showDefault <> value "logarchive" <> help "S3 bucket")
  <*> strOption (long "input-prefix" <> showDefault <> value "papertrail/logs/" <> help "prefix to input files")
  <*> strOption (long "rollup-prefix" <> showDefault <> value "papertrail/rollup/" <> help "prefix to output files")

unlessExists :: MonadIO m => FilePath -> m () -> m ()
unlessExists fn a = (liftIO . doesFileExist) fn >>= flip unless a

main' :: AWS.Env -> Options -> IO ()
main' env Options{..} = do
  before <- formatTime defaultTimeLocale "%Y-%m" <$> getCurrentTime
  runResourceT $
    runConduit $ paginate env (newListObjectsV2 bucketName & #prefix ?~ inputPrefix)
      .| CL.concatMap (view (#contents . _Just))
      .| CL.map       (view #key)
      .| CL.filter    (\v -> dt (toText v) < pack before) -- Don't include current month
      .| CL.iterM     dl                                  -- Locally cache all the files
      .| CL.groupBy   (\x y -> ym x == ym y)              -- Group them by month
      .| CL.map       (\g -> (ym . head $ g, sort g))     -- Paired by yyyy-mm
      .| CL.iterM     sevenup                             -- build/upload month .7z file
      .| CL.iterM     cleanupLocal
      .| CL.iterM     cleanupRemote
      .| CL.sinkNull -- Don't need a result here.

      where dt = Data.Text.drop 3 . head . filter ("dt=" `isPrefixOf`) . splitOn "/"
            ym = Data.Text.take 7 . dt . toText

            lfn :: ObjectKey -> FilePath
            lfn k = unpack (ym k) <> "/" <> (unpack . dropEnd 3 . last . splitOn "/" . toText $ k)

            sevenup :: (Text, [ObjectKey]) -> ResourceT IO ()
            sevenup (g, ls) = do
              let zf = unpack g <> ".7z"
                  k = ObjectKey (outputPrefix <> pack zf)
              liftIO $ sevenz zf (map lfn ls)
              hf <- hashedFile zf
              void . send env $ newPutObject bucketName k (toBody hf)

            sevenz :: FilePath -> [FilePath] -> IO ()
            sevenz fn contents = unlessExists fn $ callProcess "7z" ("a" : fn : contents)

            cleanupLocal :: (Text, [ObjectKey]) -> ResourceT IO ()
            cleanupLocal (gn, _) = liftIO $ removeDirectoryRecursive (unpack gn)

            cleanupRemote :: (Text, [ObjectKey]) -> ResourceT IO ()
            cleanupRemote (_, obs) =
              void . send env $ newDeleteObjects bucketName (newDelete & #objects .~ map newObjectIdentifier obs)

            -- dl downloads files that don't yet exist locally
            dl :: ObjectKey -> ResourceT IO ()
            dl k = let fn = lfn k in unlessExists fn $ dl' fn

                where dl' fn =  do
                        liftIO $ putStrLn ( "downloading " <> fn)
                        liftIO $ createDirectoryIfMissing False $ unpack (ym k)
                        rs <- send env (newGetObject bucketName k)
                        (rs ^. #body) `sinkBody` (ungzip .| CB.sinkFile (fn <> ".tmp"))
                        liftIO $ renameFile (fn <> ".tmp") fn

main :: IO ()
main = do
    env <- newEnv discover <&> set #region NorthVirginia
    main' env =<< execParser opts

  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "Process papertrail logs.")
