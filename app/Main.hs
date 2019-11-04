{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Lens
import           Control.Monad                (unless, void)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Trans.AWS      (AWST, Credentials (..),
                                               Region (..), ToBody (..),
                                               envRegion, hashedFile, newEnv,
                                               paginate, runAWST, runResourceT,
                                               send, sinkBody)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Conduit                 (runConduit, (.|))
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.List            as CL
import           Data.Conduit.Zlib            (ungzip)
import           Data.List                    (sort)
import           Data.Semigroup               ((<>))
import           Data.Text                    (Text, drop, dropEnd, isPrefixOf,
                                               pack, splitOn, take, unpack)
import           Data.Time                    (defaultTimeLocale, formatTime,
                                               getCurrentTime)
import           Network.AWS.Data             (toText)
import           Network.AWS.S3
import           Options.Applicative          (Parser, execParser, fullDesc,
                                               help, helper, info, long,
                                               progDesc, showDefault, strOption,
                                               value, (<**>))
import           System.Directory             (createDirectoryIfMissing,
                                               doesFileExist,
                                               removeDirectoryRecursive,
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

exists :: MonadIO m => FilePath -> m Bool
exists = liftIO . doesFileExist

main' :: Options -> IO ()
main' Options{..} = do
  before <- formatTime defaultTimeLocale "%Y-%m" <$> getCurrentTime
  env <- newEnv Discover <&> set envRegion NorthVirginia
  runResourceT . runAWST env $
    runConduit $ paginate (listObjectsV2 bucketName & lovPrefix .~ Just inputPrefix)
      .| CL.concatMap (view lovrsContents)
      .| CL.map       (view oKey)
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

            sevenup :: (Text, [ObjectKey]) -> AWST (ResourceT IO) ()
            sevenup (g, ls) = do
              let zf = (unpack g <> ".7z")
                  k = ObjectKey (outputPrefix <> pack zf)
              liftIO $ sevenz zf (map lfn ls)
              hf <- hashedFile zf
              void . send $ putObject bucketName k (toBody hf)

            sevenz :: FilePath -> [FilePath] -> IO ()
            sevenz fn contents = exists fn >>= \e -> unless e $ callProcess "7z" ("a" : fn : contents)

            cleanupLocal :: (Text, [ObjectKey]) -> AWST (ResourceT IO) ()
            cleanupLocal (gn, _) = liftIO $ removeDirectoryRecursive (unpack gn)

            cleanupRemote :: (Text, [ObjectKey]) -> AWST (ResourceT IO) ()
            cleanupRemote (_, obs) =
              void . send $ deleteObjects bucketName (delete' & dObjects .~ map objectIdentifier obs)

            -- dl downloads files that don't yet exist locally
            dl :: ObjectKey -> AWST (ResourceT IO) ()
            dl k = do
              let fn = lfn k
              e <- exists fn
              unless e $ dl' fn

                where dl' fn =  do
                        liftIO $ putStrLn ( "downloading " <> fn)
                        liftIO $ createDirectoryIfMissing False $ unpack (ym k)
                        rs <- send $ (getObject bucketName k)
                        (rs ^. gorsBody) `sinkBody` (ungzip .| CB.sinkFile (fn <> ".tmp"))
                        liftIO $ renameFile (fn <> ".tmp") fn

main :: IO ()
main = main' =<< execParser opts

  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "Process papertrail logs.")
