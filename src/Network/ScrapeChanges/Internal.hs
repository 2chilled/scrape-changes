module Network.ScrapeChanges.Internal (
  defaultScrapeConfig
, validateScrapeConfig
, validateCronSchedule
, hash
, readLatestHash
, saveHash
, executeCallbackConfig
) where
import Prelude hiding (filter)
import Data.Validation
import Data.List.NonEmpty hiding (head, tail)
import Data.ByteString.Strict.Lens
import Data.Functor (($>))
import Control.Lens
import qualified Network.URI as U
import qualified Data.Foldable as F
import Network.ScrapeChanges.Internal.Domain
import qualified Data.ByteString.Lens as ByteStringLens
import qualified Data.Text.Lens as TextLens
import qualified Text.Email.Validate as EmailValidate
import qualified Data.Attoparsec.Text as AttoparsecText
import qualified System.Cron.Parser as CronParser
import qualified Data.Digest.CRC32 as CRC32
import Control.Monad (void)
import qualified Data.Hashable as Hashable
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Network.Mail.Mime as Mime
import System.FilePath ((</>))

invalidMailAddr :: MailAddr
invalidMailAddr = MailAddr { _mailAddrName = Nothing, _mailAddr = "invalidmail" }

-- TODO require scrapeInfoUrl, mailFrom, mailTo
defaultScrapeConfig :: ScrapeConfig t
defaultScrapeConfig = ScrapeConfig {
  _scrapeInfoUrl = ""
, _scrapeInfoCallbackConfig = MailConfig defaultMail
} where defaultMail :: Mail
        defaultMail = Mail {
          _mailFrom = invalidMailAddr
        , _mailTo = invalidMailAddr :| []
        , _mailSubject = ""
        , _mailBody = ""
        }

validateScrapeConfig :: ScrapeConfig t -> ScrapeValidation (ScrapeConfig t)
validateScrapeConfig si = 
  let toUnit = void
      urlValidation = validateUrl $ si ^. scrapeInfoUrl
      callbackValidation = validateCallbackConfig $ si ^. scrapeInfoCallbackConfig
  in const si <$> F.sequenceA_ [toUnit urlValidation, toUnit callbackValidation]

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

validateCallbackConfig :: CallbackConfig t -> ScrapeValidation (CallbackConfig t)
validateCallbackConfig (MailConfig m) = MailConfig <$> validateMailConfig m
validateCallbackConfig c@(OtherConfig _) = pure c

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

validateCronSchedule :: CronSchedule -> ScrapeValidation CronSchedule
validateCronSchedule c = 
  let mapFailure = _Failure %~ \s -> [CronScheduleInvalid s]
      setSuccess = _Success .~ c
      either' = AttoparsecText.parseOnly CronParser.cronSchedule (c ^. TextLens.packed)
      mappedEither' = mapFailure . setSuccess $ either'
  in  mappedEither' ^. _AccValidation

hash :: String -> Hash
hash s = let packedS = s ^. packedChars
             h = CRC32.crc32 packedS
         in show h 

type Hash = String

hashPath :: Hash -> IO FilePath
hashPath hash' = let fileName = FilePath.pathSeparator : hash' ++ ".hash"
                     buildHashPath p = p </> fileName
                     hashPath' = buildHashPath <$> Directory.getAppUserDataDirectory "scrape-changes"
                 in  hashPath' >>= readFile  

readLatestHash :: (Hashable.Hashable t) => t -> IO Hash
readLatestHash t = hashPath (show . Hashable.hash $ t) >>= readFile

saveHash :: (Hashable.Hashable t) => t -> Hash -> IO ()
saveHash t hash' = let hashOfT = (show . Hashable.hash $ t)
                       hashPathForT = hashPath hashOfT
                   in  hashPathForT >>= flip writeFile hash'

executeCallbackConfig :: CallbackConfig t -> String -> IO ()
executeCallbackConfig (MailConfig m) result = let m' = set mailBody result m
                                                  mimeMail = toMimeMail m'
                                              in Mime.renderSendMail mimeMail
executeCallbackConfig (OtherConfig f) result = f result $> ()

toMimeMail :: Mail -> Mime.Mail
toMimeMail m = let toMimeAddress' ms = toList $ toMimeAddress <$> ms
                   mailToMime = toMimeAddress' $ m ^. mailTo
                   mailFromMime = toMimeAddress $ m ^. mailFrom
                   mailSubjectMime = m ^. mailSubject . TextLens.packed
                   mailBodyMime = m ^. mailBody . TextLens.packed
                   mimeMail = Mime.simpleMail' (head mailToMime) mailFromMime mailSubjectMime mailBodyMime
               in mimeMail { Mime.mailTo = Mime.mailTo mimeMail ++ tail mailToMime }

toMimeAddress :: MailAddr -> Mime.Address
toMimeAddress a = Mime.Address {
  Mime.addressName = a ^? mailAddrName . _Just . TextLens.packed
, Mime.addressEmail = a ^. mailAddr . TextLens.packed
}
