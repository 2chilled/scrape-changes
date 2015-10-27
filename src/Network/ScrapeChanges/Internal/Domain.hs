{-# LANGUAGE TemplateHaskell #-}
module Network.ScrapeChanges.Internal.Domain where
import Control.Lens
import Data.Validation
import Data.List.NonEmpty
data MailAddr = MailAddr {
  _mailAddrName :: Maybe String
, _mailAddr :: String
} deriving (Show, Eq)

data Mail = Mail {
  _mailFrom :: NonEmpty MailAddr
, _mailTo :: NonEmpty MailAddr
, _mailSubject :: String
, _mailBody :: String
} deriving (Show, Eq)

data CallbackConfig t = MailConfig Mail | OtherConfig (String -> IO t)

instance Show (CallbackConfig t) where
  show (MailConfig mail) = "CallbackConfig (" ++ show mail ++ ")"
  show (OtherConfig _) = "CallbackConfig (OtherConfig (String -> IO t))"

instance Eq (CallbackConfig t) where
  (MailConfig _) == (OtherConfig _) = False
  (OtherConfig _) == (MailConfig _) = False
  (OtherConfig _) == (OtherConfig _) = False
  (MailConfig m1) == (MailConfig m2) = m1 == m2

-- TODO add cron configuration
data ScrapeInfo t = ScrapeInfo {
  _scrapeInfoUrl :: String
, _scrapeInfoCallbackConfig :: CallbackConfig t
} deriving (Show, Eq)

data ValidationError = UrlNotAbsolute 
                     | UrlProtocolInvalid 
                     | MailConfigInvalidMailFromAddr String
                     | MailConfigInvalidMailToAddr String
                       deriving (Show, Eq)

type ScrapeValidation t = AccValidation [ValidationError] t

makeLenses ''MailAddr
makeLenses ''Mail
makeLenses ''ScrapeInfo
makePrisms ''CallbackConfig
