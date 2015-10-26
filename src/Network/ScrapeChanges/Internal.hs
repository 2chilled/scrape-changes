module Network.ScrapeChanges.Internal (
  MailAddr(..)
, Mail(..)
, ScrapeInfo(..)
, CallbackConfig(..)
, defaultScrapeInfo
, validateScrapeInfo
, scrapeInfoUrl
, scrapeInfoCallbackConfig
, mailFrom
, mailTo
, mailSubject
, mailBody
, mailAddrName
, mailAddr
, _MailConfig
, _OtherConfig
, ValidationError(..)
) where
import Data.Validation
import Control.Lens
import qualified Network.URI as U
import qualified Data.Foldable as F
import Network.ScrapeChanges.Internal.Domain
import qualified Data.ByteString.Lens as ByteStringLens
import qualified Text.Email.Validate as EmailValidate

defaultScrapeInfo :: ScrapeInfo t
defaultScrapeInfo = ScrapeInfo {
  _scrapeInfoUrl = ""
, _scrapeInfoCallbackConfig = MailConfig defaultMail
} where defaultMail :: Mail
        defaultMail = Mail {
          _mailFrom = []
        , _mailTo = []
        , _mailSubject = ""
        , _mailBody = ""
        }

validateScrapeInfo :: ScrapeInfo t -> ScrapeValidation (ScrapeInfo t)
validateScrapeInfo si = 
  let toUnit x = const () <$> x
      urlValidation = validateUrl $ si ^. scrapeInfoUrl
      callbackValidation = validateCallbackConfig $ si ^. scrapeInfoCallbackConfig
  in const si <$> F.sequenceA_ [toUnit urlValidation, toUnit callbackValidation]

validateMailConfig :: Mail -> ScrapeValidation Mail
validateMailConfig m = let fromIsEmpty = null $ m ^. mailFrom
                           mailFromAddrs = m ^.. (mailFrom . traverse . mailAddr)
                           isInvalidMailAddr = (not . EmailValidate.isValid . (^. ByteStringLens.packedChars))
                           invalidMailFromAddrs = MailConfigInvalidMailFromAddr <$> (isInvalidMailAddr `filter` mailFromAddrs)
                           ok = pure m
                       in const m <$> F.sequenceA_ [
                         if fromIsEmpty then AccFailure [MailConfigEmptyFrom] else ok
                       , if null invalidMailFromAddrs then ok else AccFailure invalidMailFromAddrs
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
