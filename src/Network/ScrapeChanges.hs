{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Network.ScrapeChanges 
-- Copyright   : (C) 2015-16 Matthias Herrmann
-- License     : GPL-3
-- Maintainer  : matthias.mh.herrmann@gmail.com
--
module Network.ScrapeChanges(
  scrape
, repeatScrape
, repeatScrapeAll
, scrapeAll
, ScrapeConfig(..)
, ScrapeSchedule(..)
, mailScrapeConfig
, otherScrapeConfig
, clearScrapeConfig
, ScrapeInfoUrl
, MailFromAddr
, MailToAddr
, Scraper
, ScrapeResult(..)
, Url
, Hash
, HttpBody
, scrapeScheduleCron
, scrapeScheduleConfig
, scrapeScheduleScraper
, module Domain
) where

import Network.ScrapeChanges.Internal as Internal
import Network.ScrapeChanges.Domain as Domain
import qualified Data.Validation as Validation
import qualified Data.Tuple as TU
import qualified System.Cron.Schedule as CronSchedule
import Control.Lens
import qualified Data.ByteString.Lazy as ByteString
import qualified Network.Wreq as Http
import qualified Control.Concurrent.Async as Async
import qualified System.Log.Logger as Log
import Data.Hashable (Hashable)
import qualified Data.Foldable as Foldable
--import Data.List.NonEmpty (NonEmpty (..))
--import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Traversable as Traversable
import qualified Control.Monad as Monad
import qualified Data.Tuple as Tuple

type Url = String
type HttpBody = ByteString.ByteString
type Scraper t = HttpBody -> t
data ScrapeResult t = CallbackCalled t | CallbackNotCalled t deriving Show
data ScrapeSchedule t = ScrapeSchedule { _scrapeScheduleCron :: CronSchedule
                                       , _scrapeScheduleConfig :: ScrapeConfig t
                                       , _scrapeScheduleScraper :: Scraper t
                                       }                 
makeLenses ''ScrapeSchedule

-- TODO investigate utf-8 issue
-- |The basic scrape function. It fires a GET request against the url
-- defined within the provided 'ScrapeConfig'. The body is passed to the
-- provided 'Scraper'. The result 't' of the latter is used to determine
-- whether something has changed on the respective website. If so,
-- the callback configured in 'ScrapeConfig' is executed and 
-- 'CallbackCalled' is returned. Otherwise 'CallbackNotCalled' is returned.
--
-- Note that you should call 'clearScrapeConfig' after executing the
-- returned 'IO' action with the same 'ScrapeConfig' you provided to this
-- function. This will clear all mutable state used by 'scrape'.
scrape :: (Hashable t, Show t) => ScrapeConfig t -> Scraper t -> Either [ValidationError] (IO (ScrapeResult t))
scrape sc s = let result = scrapeOrchestration <$ validateScrapeConfig sc
              in result ^. Validation._Either
  where scrapeOrchestration = 
          let unpackResponse = (^. Http.responseBody)
              urlToRequest = sc ^. scrapeInfoUrl
              requestLog = Log.infoM loggerName $ "Requesting " ++ urlToRequest
              request = (s . unpackResponse <$>) . Http.get
              response = request urlToRequest <* requestLog
          in do (response', latestHashedResponse) <- Async.concurrently response (readLatestHash sc)
                let currentHashedResponse = hash' response'
                let hashesAreDifferent = Maybe.isNothing latestHashedResponse 
                                          || Foldable.or ((/= currentHashedResponse) <$> latestHashedResponse)
                let saveHash' = let saveHashMsg = "Saved new hash for url '" ++ urlToRequest ++ "'"
                                    saveHashLog = Log.infoM loggerName saveHashMsg
                                in  saveHash sc currentHashedResponse <* saveHashLog
                let executeCallbackConfig' = executeCallbackConfig sc response'
                let saveHashAndExecuteCallbackConfig = Async.concurrently saveHash' executeCallbackConfig'
                if hashesAreDifferent then CallbackCalled response' <$ saveHashAndExecuteCallbackConfig 
                                      else pure $ CallbackNotCalled response'

-- |Repeat executing 'scrape' by providing a 'CronSchedule'. The returned
-- IO action blocks the current thread
repeatScrape :: (Hashable t, Show t) => CronSchedule -> ScrapeConfig t -> Scraper t -> Either [ValidationError] (IO ())
repeatScrape cs sc s = let result = repeatScrapeAll [ScrapeSchedule cs sc s]
                           resultErrorMapped = (snd . head <$> (result ^. swapped)) ^. swapped
                       in resultErrorMapped

repeatScrapeAll :: (Hashable t, Show t) => [ScrapeSchedule t] -> Either [(Url, [ValidationError])] (IO ())
repeatScrapeAll scrapeSchedules = 
  let cronSchedules = Traversable.for scrapeSchedules $ \(ScrapeSchedule cronSchedule scrapeConfig scraper) ->
        let scrapeConfigUrl = scrapeConfig ^. scrapeInfoUrl
            cronScheduleValidation = validateCronSchedule cronSchedule
            resultValidation = scrape scrapeConfig scraper ^. Validation._AccValidation
            resultDroppedT = Monad.void <$> resultValidation
            resultWithCronSchedule = (,) <$> resultDroppedT <*> cronScheduleValidation
            resultWithCronScheduleErrorMapped = 
              ((\x -> [(scrapeConfigUrl, x)]) <$> (resultWithCronSchedule ^. swapped)) ^. swapped
        in  Tuple.uncurry CronSchedule.addJob <$> resultWithCronScheduleErrorMapped
  in (Monad.void . CronSchedule.execSchedule . Foldable.sequenceA_) <$> cronSchedules ^. Validation._Either

-- |Execute a list of 'ScrapeConfig' in sequence using 'scrape' and collect
-- the results in a map containing the respective 'Url' as key.
scrapeAll :: (Hashable t, Show t) => [(ScrapeConfig t, Scraper t)] -> [(Url, Either [ValidationError] (IO (ScrapeResult t)))]
scrapeAll infos = let responses = TU.uncurry scrape <$> infos 
                      urls = (^. scrapeInfoUrl) <$> (fst <$> infos)
                  in urls `zip` responses

-- |Clear all mutable state associated with the provided 'ScrapeConfig'
clearScrapeConfig :: (Hashable t) => ScrapeConfig t -> IO ()
clearScrapeConfig = removeHash

-- private 

{-
 main :: IO ()
 main = let url = "http://www.bodman-ludwigshafen.de/verwaltung/bauen-und-planen/"
            mailFrom = MailAddr Nothing "scraper@matthias01.bestforever.com"
            mailTo = MailAddr Nothing "matthias.mh.herrmann@gmail.com"
            scrapeConfig = mailScrapeConfig url mailFrom (mailTo :| [])
            cronSchedule = "* * * * *"
            scraper = ByteString.unpack
            scrapeChangeResult = scrape scrapeConfig scraper
        in  (Either.either print id) $ repeatScrape cronSchedule scrapeConfig scraper -- (Either.either print (>>= print)) $ scrape scrapeConfig scraper -- 
 -}
