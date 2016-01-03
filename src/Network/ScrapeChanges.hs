module Network.ScrapeChanges(
  scrape
, repeatScrape
, scrapeAll
, ScrapeConfig(..)
, mailScrapeConfig
, otherScrapeConfig
, clearScrapeConfig
) where

import Network.ScrapeChanges.Internal as Internal
import Network.ScrapeChanges.Internal.Domain as Domain
import qualified Data.Validation as Validation
import qualified Data.Tuple as TU
import qualified System.Cron.Schedule as CronSchedule
import Control.Lens
import qualified Data.ByteString.Lazy.Internal as ByteString
import qualified Network.Wreq as Http
import qualified Control.Concurrent.Async as Async
import qualified System.Log.Logger as Log
import Data.Hashable (Hashable)

type Url = String
type Scraper t = ByteString.ByteString -> t

data ScrapeResult t = CallbackCalled t | CallbackNotCalled t

scrape :: Hashable t => ScrapeConfig t -> Scraper t -> Either [ValidationError] (IO (ScrapeResult t))
scrape sc s = let result = scrapeOrchestration <$ validateScrapeConfig sc
              in result ^. Validation._Either
  where scrapeOrchestration = 
          let unpackResponse = (^. Http.responseBody)
              urlToRequest = sc ^. scrapeInfoUrl
              requestLog = Log.infoM thisModule $ "Requesting " ++ urlToRequest
              request = ((s . unpackResponse <$>) . Http.get) 
              response = request urlToRequest <* requestLog
          in do (response', latestHashedResponse) <- Async.concurrently response (readLatestHash sc)
                let currentHashedResponse = hash' response'
                let hashesAreDifferent = latestHashedResponse /= currentHashedResponse
                let saveHash' = let saveHashLog = Log.infoM thisModule $ "Saved new hash for url '" ++ urlToRequest ++ "'"
                                in  saveHash sc latestHashedResponse <* saveHashLog
                let executeCallbackConfig' = executeCallbackConfig (sc ^. scrapeInfoCallbackConfig) response'
                let saveHashAndExecuteCallbackConfig = Async.concurrently saveHash' executeCallbackConfig'
                if hashesAreDifferent then CallbackCalled response' <$ saveHashAndExecuteCallbackConfig 
                                      else pure $ CallbackNotCalled response'

repeatScrape :: Hashable t => CronSchedule -> ScrapeConfig t -> Scraper t -> Either [ValidationError] (IO ())
repeatScrape cs sc s = 
  let cronSchedule = validateCronSchedule cs
      scrapeResult = scrape sc s ^. Validation._AccValidation
      scrapeResultDroppedT = (fmap . fmap $ const ()) scrapeResult
      scrapeResultRepeated = repeatScrape' <$ cronSchedule <*> scrapeResultDroppedT
  in scrapeResultRepeated ^. Validation._Either
  where repeatScrape' :: IO () -> IO ()
        repeatScrape' scrapeAction = () <$ CronSchedule.execSchedule (CronSchedule.addJob scrapeAction cs)

scrapeAll :: Hashable t => [(ScrapeConfig t, Scraper t)] -> [(Url, Either [ValidationError] (IO (ScrapeResult t)))]
scrapeAll infos = let responses = TU.uncurry scrape <$> infos 
                      urls = (^. scrapeInfoUrl) <$> (fst <$> infos)
                  in urls `zip` responses
-- private 

clearScrapeConfig :: (Hashable t) => ScrapeConfig t -> IO ()
clearScrapeConfig = removeHash

thisModule :: String
thisModule = "Network.ScrapeChanges"
