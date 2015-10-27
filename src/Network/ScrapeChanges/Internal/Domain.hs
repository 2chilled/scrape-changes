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
                     | MailConfigInvalidMailFromAddr String
                       deriving (Show, Eq)

type ScrapeValidation t = AccValidation [ValidationError] t

makeLenses ''MailAddr
makeLenses ''Mail
makeLenses ''ScrapeInfo
makeLenses ''CallbackConfig
makePrisms ''CallbackConfig
