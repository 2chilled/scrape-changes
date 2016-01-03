module Network.ScrapeChanges(
  scrape
, repeatScrape
, scrapeAll
, ScrapeConfig(..)
, mailScrapeConfig
) where
import Network.ScrapeChanges.Internal as Internal
import Network.ScrapeChanges.Internal.Domain as Domain
import qualified Data.Validation as Validation
import qualified Data.Tuple as TU
import qualified System.Cron.Schedule as CronSchedule
import Control.Lens
import qualified Data.ByteString.Lazy.Internal as ByteString
import qualified Network.Wreq as Http
import Control.Monad (when)
import qualified Control.Concurrent.Async as Async
import qualified System.Log.Logger as Log
import Data.Hashable (Hashable)
import qualified Data.Hashable as Hashable

type Url = String
type Scraper t = ByteString.ByteString -> t

-- TODO 
-- add a function for cleaning up the written hashes
-- review the hash functions

scrape :: Hashable t => ScrapeConfig t -> Scraper t -> Either [ValidationError] (IO t)
scrape sc s = let result = const scrapeOrchestration <$> validateScrapeConfig sc
              in result ^. Validation._Either
  where scrapeOrchestration = 
          let unpackResponse = (^. Http.responseBody)
              urlToRequest = sc ^. scrapeInfoUrl
              requestLog = Log.infoM thisModule $ "Requesting " ++ urlToRequest
              request = ((s . unpackResponse <$>) . Http.get) 
              response = request urlToRequest <* requestLog
          in do (response', latestHashedResponse) <- Async.concurrently response (readLatestHash sc)
                let currentHashedResponse = show . Hashable.hash $ response'
                let hashesAreDifferent = latestHashedResponse /= currentHashedResponse
                let saveHashIfSomethingHasChanged = let saveHashLog = Log.infoM thisModule $ "Saved new hash for url '" ++ urlToRequest ++ "'"
                                                        saveHash' = saveHash sc latestHashedResponse <* saveHashLog
                                                    in  when hashesAreDifferent saveHash' 
                let executeCallbackConfig' = executeCallbackConfig (sc ^. scrapeInfoCallbackConfig) response'
                _ <- Async.concurrently saveHashIfSomethingHasChanged executeCallbackConfig'
                return response'

repeatScrape :: Hashable t => CronSchedule -> ScrapeConfig t -> Scraper t -> Either [ValidationError] (IO ())
repeatScrape cs sc s = 
  let cronSchedule = validateCronSchedule cs
      scrapeResult = scrape sc s ^. Validation._AccValidation
      scrapeResultDroppedT = (fmap . fmap $ const ()) scrapeResult
      scrapeResultRepeated = repeatScrape' <$ cronSchedule <*> scrapeResultDroppedT
  in scrapeResultRepeated ^. Validation._Either
  where repeatScrape' :: IO () -> IO ()
        repeatScrape' scrapeAction = () <$ CronSchedule.execSchedule (CronSchedule.addJob scrapeAction cs)

scrapeAll :: Hashable t => [(ScrapeConfig t, Scraper t)] -> [(Url, Either [ValidationError] (IO t))]
scrapeAll infos = let responses = TU.uncurry scrape <$> infos 
                      urls = (^. scrapeInfoUrl) <$> (fst <$> infos)
                  in urls `zip` responses
-- private 

thisModule :: String
thisModule = "Network.ScrapeChanges"
