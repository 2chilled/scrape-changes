{-# LANGUAGE TemplateHaskell #-}
module Network.ScrapeChanges.Internal.Domain where
import Control.Lens
import Data.Validation
import Data.List.NonEmpty
import Data.Hashable (Hashable, hashWithSalt)

data MailAddr = MailAddr {
  _mailAddrName :: Maybe String
, _mailAddr :: String
} deriving (Show, Eq)

data Mail = Mail {
  -- TODO should be single value
  _mailFrom :: NonEmpty MailAddr
, _mailTo :: NonEmpty MailAddr
, _mailSubject :: String
, _mailBody :: String
} deriving (Show, Eq)

-- TODO probably we can remove the type variable
data CallbackConfig t = MailConfig Mail | OtherConfig (String -> IO t)

instance Show (CallbackConfig t) where
  show (MailConfig mail) = "CallbackConfig (" ++ show mail ++ ")"
  show (OtherConfig _) = "CallbackConfig (OtherConfig (String -> IO t))"

instance Eq (CallbackConfig t) where
  (MailConfig _) == (OtherConfig _) = False
  (OtherConfig _) == (MailConfig _) = False
  (OtherConfig _) == (OtherConfig _) = False
  (MailConfig m1) == (MailConfig m2) = m1 == m2

data ScrapeConfig t = ScrapeConfig {
  _scrapeInfoUrl :: String
, _scrapeInfoCallbackConfig :: CallbackConfig t
} deriving (Show, Eq)

instance Hashable (ScrapeConfig t) where
  hashWithSalt i c = hashWithSalt i (show c)

data ValidationError = UrlNotAbsolute 
                     | UrlProtocolInvalid 
                     | MailConfigInvalidMailFromAddr String
                     | MailConfigInvalidMailToAddr String
                     | CronScheduleInvalid String
                       deriving (Show, Eq)

type ScrapeValidation t = AccValidation [ValidationError] t

type CronSchedule = String

makeLenses ''MailAddr
makeLenses ''Mail
makeLenses ''ScrapeConfig
makePrisms ''CallbackConfig
