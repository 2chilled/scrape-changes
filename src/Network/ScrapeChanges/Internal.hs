{-# LANGUAGE TemplateHaskell #-}
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

data MailAddr = MailAddr {
  _mailAddrName :: Maybe String
, _mailAddr :: String
} deriving (Show, Eq)

data Mail = Mail {
  _mailFrom :: [MailAddr]
, _mailTo :: [MailAddr]
, _mailSubject :: String
, _mailBody :: String
} deriving (Show, Eq)

data CallbackConfig t = MailConfig Mail | OtherConfig (String -> IO t)

instance Show (CallbackConfig t) where
  show (MailConfig mail) = show mail
  show (OtherConfig _) = "OtherConfig (String -> IO t)"

instance Eq (CallbackConfig t) where
  (MailConfig _) == (OtherConfig _) = False
  (OtherConfig _) == (MailConfig _) = False
  (OtherConfig _) == (OtherConfig _) = False
  (MailConfig m1) == (MailConfig m2) = m1 == m2

data ScrapeInfo t = ScrapeInfo {
  _scrapeInfoUrl :: String
, _scrapeInfoCallbackConfig :: CallbackConfig t
} deriving (Show, Eq)

data ValidationError = UrlNotAbsolute 
                     | UrlProtocolInvalid 
                     | MailConfigEmptyFrom 
                       deriving (Show, Eq)

type ScrapeValidation t = AccValidation [ValidationError] t

makeLenses ''MailAddr
makeLenses ''Mail
makeLenses ''ScrapeInfo
makeLenses ''CallbackConfig
makePrisms ''CallbackConfig

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
                           ok = pure m
                       in const m <$> F.sequenceA_ [
                         if fromIsEmpty then AccFailure [MailConfigEmptyFrom] else ok
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
