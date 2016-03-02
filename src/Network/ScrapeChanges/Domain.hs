{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
{-# OPTIONS_HADDOCK not-home #-}

module Network.ScrapeChanges.Domain where

import Control.Lens
import Data.Validation
import Data.List.NonEmpty
import Data.Hashable (Hashable, hashWithSalt)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy as TextLazy
import GHC.Generics (Generic)

-- |String encoded in the standard cron format
type CronScheduleString = String

-- |Url to scrape
type Url = String
-- |Body of the HTTP request
type HttpBody = ByteString
-- |Function extracting 'Text' of 'HttpBody'
type Scraper = HttpBody -> Text
-- |Codomain of 'Scraper'
type Text = TextLazy.Text

-- |Mail address for provided 'MailConfig'
data MailAddr = MailAddr {
  -- |Optional name for the given '_mailAddr'
  _mailAddrName :: Maybe Text
  -- |Mail address
, _mailAddr :: String
} deriving (Show, Eq, Generic)

makeLenses ''MailAddr

instance Hashable MailAddr

data Mail = Mail {
  _mailFrom :: MailAddr
, _mailTo :: NonEmpty MailAddr
, _mailSubject :: Text
, _mailBody :: Text
} deriving (Show, Eq, Generic)

makeLenses ''Mail

instance Hashable Mail

data CallbackConfig
    -- |Send a mail when there's changed data at your scrape target.
    -- This needs sendmail to be configured correctly on the host your
    -- program runs. 
    = MailConfig Mail 
    -- |Just execute the provided function when there's changed data at
    -- your scrape target.
    | OtherConfig (Text -> IO ())

makePrisms ''CallbackConfig

instance Eq CallbackConfig where
  (MailConfig _) == (OtherConfig _) = False
  (OtherConfig _) == (MailConfig _) = False
  (OtherConfig _) == (OtherConfig _) = False
  (MailConfig m1) == (MailConfig m2) = m1 == m2

data ScrapeConfig = ScrapeConfig {
  -- |The url to be called using GET
  _scrapeInfoUrl :: String
  -- |The callback config to be executed when something in '_scrapeInfoUrl'
  -- has changed
, _scrapeInfoCallbackConfig :: CallbackConfig 
} deriving (Eq)

makeLenses ''ScrapeConfig

instance Hashable ScrapeConfig where
  hashWithSalt i c = 
    let scrapeInfoUrlHash = hashWithSalt i (c ^. scrapeInfoUrl)
        mailConfigHash = hashWithSalt i (c ^? scrapeInfoCallbackConfig . _MailConfig)
    in scrapeInfoUrlHash + mailConfigHash

data ScrapeResult = CallbackCalled | CallbackNotCalled deriving Show
data ScrapeSchedule = ScrapeSchedule { _scrapeScheduleCron :: CronScheduleString
                                     , _scrapeScheduleConfig :: ScrapeConfig
                                     , _scrapeScheduleScraper :: Scraper
                                     }                 
makeLenses ''ScrapeSchedule

data ValidationError = UrlNotAbsolute 
                     | UrlProtocolInvalid 
                     | MailConfigInvalidMailFromAddr String
                     | MailConfigInvalidMailToAddr String
                     | CronScheduleInvalid String
                       deriving (Show, Eq)

type ScrapeValidation t = AccValidation [ValidationError] t

