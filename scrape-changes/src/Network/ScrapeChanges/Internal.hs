{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.ScrapeChanges.Internal (
  mailScrapeConfig
, otherScrapeConfig
, validateScrapeConfig
, validateCronSchedule
, readLatestHash
, saveHash
, executeCallbackConfig
, removeHash
, removeHashes
, hash'
, ScrapeInfoUrl
, MailFromAddr
, MailToAddr
, Hash
, loggerName
, httpExceptionHandler
) where

import Prelude hiding (filter)
import Data.Validation
import Data.List.NonEmpty hiding (head, tail)
import Data.Functor (($>))
import Control.Lens
import qualified Control.Exception as Exception
import qualified Network.URI as U
import qualified Data.Foldable as F
import Network.ScrapeChanges.Domain
import qualified Data.ByteString.Lens as ByteStringLens
import qualified Text.Email.Validate as EmailValidate
import qualified Data.Attoparsec.Text as AttoparsecText
import qualified System.Cron.Parser as CronParser
import Control.Monad (void)
import qualified Data.Hashable as Hashable
import Data.Hashable (Hashable)
import qualified System.Directory as Directory
import qualified Network.Mail.Mime as Mime
import System.FilePath ((</>))
import qualified System.IO.Error as IOError
import qualified System.Directory as Dir
import qualified System.FilePath as FilePath
import qualified System.IO.Strict as StrictIO
import qualified System.Log.Logger as Log
import qualified Data.Maybe as Maybe
import Network.HTTP.Client (HttpException)
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text as TextStrict
import qualified Data.Text.Lens as TextStrictLens

type ScrapeInfoUrl = String
type MailFromAddr = MailAddr
type MailToAddr = MailAddr

-- |Helper constructor for 'ScrapeConfig' containing 'MailConfig'
-- callback.
mailScrapeConfig :: ScrapeInfoUrl -> MailFromAddr -> NonEmpty MailToAddr -> ScrapeConfig
mailScrapeConfig siu mfa mtads = ScrapeConfig {
  _scrapeInfoUrl = siu
, _scrapeInfoCallbackConfig = MailConfig defaultMail
} where defaultMail :: Mail
        defaultMail = Mail {
          _mailFrom = mfa
        , _mailTo = mtads
        , _mailSubject = ""
        , _mailBody = ""
        }

-- |Helper constructor for 'ScrapeConfig' containing 'OtherConfig'
-- callback.
otherScrapeConfig :: ScrapeInfoUrl -> (Text -> IO ()) -> ScrapeConfig
otherScrapeConfig url f = ScrapeConfig {
  _scrapeInfoUrl = url
, _scrapeInfoCallbackConfig = OtherConfig f
}

validateScrapeConfig :: ScrapeConfig -> ScrapeValidation ScrapeConfig
validateScrapeConfig si = 
  let toUnit = void
      urlValidation = validateUrl $ si ^. scrapeInfoUrl
      callbackValidation = validateCallbackConfig $ si ^. scrapeInfoCallbackConfig
  in const si <$> F.sequenceA_ [toUnit urlValidation, toUnit callbackValidation]

validateCallbackConfig :: CallbackConfig -> ScrapeValidation CallbackConfig
validateCallbackConfig (MailConfig m) = MailConfig <$> validateMailConfig m
validateCallbackConfig c@(OtherConfig _) = pure c

validateCronSchedule :: CronScheduleString -> ScrapeValidation CronScheduleString
validateCronSchedule c = 
  let mapFailure = _Failure %~ \s -> [CronScheduleInvalid s]
      setSuccess = _Success .~ c
      either' = AttoparsecText.parseOnly CronParser.cronSchedule (TextStrict.pack c)
      mappedEither' = either' & mapFailure
                              & setSuccess
  in  mappedEither' ^. _AccValidation

type Hash = String

hashPath :: Hash -> IO FilePath
hashPath hash'' = let fileName = hash'' ++ ".hash"
                      buildHashPath p = p </> fileName
                  in  buildHashPath <$> hashPathDir

hashPathDir :: IO FilePath
hashPathDir = Directory.getAppUserDataDirectory "scrape-changes"

readLatestHash :: (Hashable t) => t -> IO (Maybe Hash)
readLatestHash t = let readLatestHash' = hashPath (hash' t) >>= StrictIO.readFile
                       readLatestHashMaybe = Just <$> readLatestHash'
                   in readLatestHashMaybe `IOError.catchIOError` (\e -> if IOError.isDoesNotExistError e then pure Nothing else ioError e)

hash' :: Hashable t => t -> String
hash' = show . Hashable.hash

removeHash :: (Hashable t) => t -> IO ()
removeHash t = ((hashPath . hash' $ t) >>= Directory.removeFile) `Exception.catch` catchException
  where catchException e | IOError.isDoesNotExistError e = return () 
                         | otherwise = Exception.throwIO e

removeHashes :: IO ()
removeHashes = let removeAction = hashPathDir >>= Directory.removeDirectory
               in Exception.catch removeAction catchAll
  where catchAll :: Exception.SomeException -> IO ()
        catchAll = const . return $ ()

executeCallbackConfig :: ScrapeConfig -> Text -> IO ()
executeCallbackConfig (ScrapeConfig url (MailConfig m)) result = 
    let m' = m & set mailBody result
               & set mailSubject (TextLazy.pack $ "Changes from " ++ url)
        mimeMail = toMimeMail m'
        debugLog = Log.debugM loggerName $ "Mail body: " ++ show m'
    in debugLog *> Mime.renderSendMail mimeMail
executeCallbackConfig (ScrapeConfig _ (OtherConfig f)) result = f result $> ()

loggerName :: String
loggerName = "Network.ScrapeChanges"

-- private

validateMailConfig :: Mail -> ScrapeValidation Mail
validateMailConfig m = 
  let mailAddrs t = fromList $ m ^.. (t . traverse . mailAddr)
      isInvalidMailAddr = (not . EmailValidate.isValid . (^. ByteStringLens.packedChars))
      mailFromAddr = m ^. mailFrom . mailAddr
      invalidMailFromAddrs = MailConfigInvalidMailFromAddr <$> [mailFromAddr | isInvalidMailAddr mailFromAddr]
      mailToAddrs = mailAddrs mailTo
      invalidMailToAddrs = MailConfigInvalidMailToAddr <$> (isInvalidMailAddr `filter` mailToAddrs)
      ok = pure m
  in const m <$> F.sequenceA_ [
    if null invalidMailFromAddrs then ok else AccFailure invalidMailFromAddrs
  , if null invalidMailToAddrs then ok else AccFailure invalidMailToAddrs
  ]


validateUrl :: String -> ScrapeValidation String
validateUrl s = let uriMaybe = U.parseAbsoluteURI s
                    isAbsoluteUrl = U.isAbsoluteURI s 
                    protocolMaybe = U.uriScheme <$> uriMaybe
                    isHttp = (=="http:") `F.all` protocolMaybe
                    ok = pure s
                in const s <$> F.sequenceA_ [
                     if isAbsoluteUrl then ok else AccFailure [UrlNotAbsolute]
                   , if isHttp then ok else AccFailure [UrlProtocolInvalid]
                   ]


saveHash :: (Hashable t) => t -> Hash -> IO ()
saveHash t hash'' = let hashOfT = hash' t
                        hashPathForT = hashPath hashOfT >>= createParentDirs
                    in  hashPathForT >>= flip writeFile hash''

toMimeMail :: Mail -> Mime.Mail
toMimeMail m = let toMimeAddress' ms = toList $ toMimeAddress <$> ms
                   mailToMime = toMimeAddress' $ m ^. mailTo
                   mailFromMime = toMimeAddress $ m ^. mailFrom
                   mailSubjectMime = m ^. mailSubject 
                   mailBodyMime = m ^. mailBody
                   mimeMail = Mime.simpleMail' (head mailToMime) mailFromMime (TextLazy.toStrict mailSubjectMime) mailBodyMime
               in mimeMail { Mime.mailTo = Mime.mailTo mimeMail ++ tail mailToMime }

toMimeAddress :: MailAddr -> Mime.Address
toMimeAddress a = Mime.Address {
  Mime.addressName = a ^? mailAddrName . _Just . to TextLazy.toStrict
, Mime.addressEmail = a ^. mailAddr . TextStrictLens.packed
}

createParentDirs :: FilePath -> IO FilePath
createParentDirs fp = let fpDir = FilePath.takeDirectory fp
                      in Dir.createDirectoryIfMissing True fpDir *> pure fp

httpExceptionHandler :: ScrapeConfig -> HttpException -> IO t
httpExceptionHandler sc e = let maybeMail = sc ^? scrapeInfoCallbackConfig . _MailConfig
                                url = sc ^. scrapeInfoUrl
                                maybeMailAction = Maybe.fromMaybe (pure ()) (sendMail url <$> maybeMail)
                            in F.sequenceA_ [Log.errorM loggerName (show e), maybeMailAction] *> Exception.throw e
                                
  where sendMail :: Url -> Mail -> IO ()
        sendMail url m = let m' = m & set mailBody (TextLazy.pack $ show e)
                                    & set mailSubject (TextLazy.pack $ "Http error while requesting " ++ url)
                             mimeMail = toMimeMail m'
                         in Mime.renderSendMail mimeMail 

--                                    saveHashLog = Log.infoM loggerName saveHashMsg
