-- |
-- Module      : Network.ScrapeChanges 
-- Copyright   : (C) 2015-16 Matthias Herrmann
-- License     : GPL-3
-- Maintainer  : matthias.mh.herrmann@gmail.com
--
module Network.ScrapeChanges(
  scrape
, repeatScrape
, scrapeAll
, ScrapeConfig(..)
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
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable

type Url = String
type HttpBody = ByteString.ByteString
type Scraper t = HttpBody -> t
data ScrapeResult t = CallbackCalled t | CallbackNotCalled t deriving Show

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
scrape :: Hashable t => ScrapeConfig t -> Scraper t -> Either [ValidationError] (IO (ScrapeResult t))
scrape sc s = let result = scrapeOrchestration <$ validateScrapeConfig sc
              in result ^. Validation._Either
  where scrapeOrchestration = 
          let unpackResponse = (^. Http.responseBody)
              urlToRequest = sc ^. scrapeInfoUrl
              requestLog = Log.infoM thisModule $ "Requesting " ++ urlToRequest
              request = (s . unpackResponse <$>) . Http.get
              response = request urlToRequest <* requestLog
          in do (response', latestHashedResponse) <- Async.concurrently response (readLatestHash sc)
                let currentHashedResponse = hash' response'
                let hashesAreDifferent = Foldable.or $ (/= currentHashedResponse) <$> latestHashedResponse
                let saveHash' = let saveHashLog = Log.infoM thisModule $ "Saved new hash for url '" ++ urlToRequest ++ "'"
                                in  saveHash sc currentHashedResponse <* saveHashLog
                let executeCallbackConfig' = executeCallbackConfig (sc ^. scrapeInfoCallbackConfig) response'
                let saveHashAndExecuteCallbackConfig = Async.concurrently saveHash' executeCallbackConfig'
                if hashesAreDifferent then CallbackCalled response' <$ saveHashAndExecuteCallbackConfig 
                                      else pure $ CallbackNotCalled response'

-- |Repeat executing 'scrape' by providing a 'CronSchedule'
repeatScrape :: Hashable t => CronSchedule -> ScrapeConfig t -> Scraper t -> Either [ValidationError] (IO ())
repeatScrape cs sc s = 
  let cronSchedule = validateCronSchedule cs
      scrapeResult = scrape sc s ^. Validation._AccValidation
      scrapeResultDroppedT = (fmap . fmap $ const ()) scrapeResult
      scrapeResultRepeated = repeatScrape' <$ cronSchedule <*> scrapeResultDroppedT
  in scrapeResultRepeated ^. Validation._Either
  where repeatScrape' :: IO () -> IO ()
        repeatScrape' scrapeAction = () <$ CronSchedule.execSchedule (CronSchedule.addJob scrapeAction cs)

-- |Execute a list of 'ScrapeConfig' in sequence using 'scrape' and collect
-- the results in a map containing the respective 'Url' as key.
scrapeAll :: Hashable t => [(ScrapeConfig t, Scraper t)] -> [(Url, Either [ValidationError] (IO (ScrapeResult t)))]
scrapeAll infos = let responses = TU.uncurry scrape <$> infos 
                      urls = (^. scrapeInfoUrl) <$> (fst <$> infos)
                  in urls `zip` responses

-- |Clear all mutable state associated with the provided 'ScrapeConfig'
clearScrapeConfig :: (Hashable t) => ScrapeConfig t -> IO ()
clearScrapeConfig = removeHash

-- private 

thisModule :: String
thisModule = "Network.ScrapeChanges"

main :: IO ()
main = let url = "http://www.bodman-ludwigshafen.de/verwaltung/bauen-und-planen/"
           mailFrom = MailAddr Nothing "scraper@matthias01.bestforever.com"
           mailTo = MailAddr Nothing "matthias.mh.herrmann@gmail.com"
           scrapeConfig = mailScrapeConfig url mailFrom (mailTo :| [])
           cronSchedule = "* * * * *"
           scraper = ByteString.unpack
           scrapeChangeResult = scrape scrapeConfig scraper
       in  Either.either print ((>>= putStrLn . show)) scrapeChangeResult
