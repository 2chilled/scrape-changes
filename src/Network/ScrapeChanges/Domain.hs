{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK not-home #-}

module Network.ScrapeChanges.Domain where
import Control.Lens
import Data.Validation
import Data.List.NonEmpty
import Data.Hashable (Hashable, hashWithSalt)

data MailAddr = MailAddr {
  _mailAddrName :: Maybe String
, _mailAddr :: String
} deriving (Show, Eq)

data Mail = Mail {
  _mailFrom :: MailAddr
, _mailTo :: NonEmpty MailAddr
, _mailSubject :: String
, _mailBody :: String
} deriving (Show, Eq)

data CallbackConfig t 
    -- |Send a mail when there's changed data at your scrape target.
    -- This needs sendmail to be configured correctly on the host your
    -- program runs. 
    = MailConfig Mail 
    -- |Just execute the provided function when there's changed data at
    -- your scrape target.
    | OtherConfig (t -> IO t)

instance Show (CallbackConfig t) where
  show (MailConfig mail) = "MailConfig (" ++ show mail ++ ")"
  show (OtherConfig _) = "OtherConfig (String -> IO t)"

instance Eq (CallbackConfig t) where
  (MailConfig _) == (OtherConfig _) = False
  (OtherConfig _) == (MailConfig _) = False
  (OtherConfig _) == (OtherConfig _) = False
  (MailConfig m1) == (MailConfig m2) = m1 == m2

data ScrapeConfig t = ScrapeConfig {
  -- |The url to be called using GET
  _scrapeInfoUrl :: String
  -- |The callback config to be executed when something in '_scrapeInfoUrl'
  -- has changed
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

-- |String encoded in the standard cron format
type CronSchedule = String

makeLenses ''MailAddr
makeLenses ''Mail
makeLenses ''ScrapeConfig
makePrisms ''CallbackConfig
