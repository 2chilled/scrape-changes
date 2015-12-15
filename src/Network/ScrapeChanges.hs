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
import Control.Lens.Internal.ByteString 
import qualified Network.Wreq as Http
import Control.Monad
import qualified Control.Concurrent.Async as Async

type Url = String
type Scraper = String -> String

scrape :: ScrapeConfig t -> Scraper -> Either [ValidationError] (IO ())
scrape sc s = let result = const scrapeOrchestration <$> validateScrapeConfig sc
              in result ^. Validation._Either
  where scrapeOrchestration = 
          let unpackResponse = unpackLazy8 . (^. Http.responseBody)
              request = (s . unpackResponse <$>) . Http.get
              response = request $ sc ^. scrapeInfoUrl
          in do (response', latestHashedResponse) <- Async.concurrently response (readLatestHash sc)
                let currentHashedResponse = hash response'
                let hashesAreDifferent = latestHashedResponse /= currentHashedResponse
                let saveHashIfSomethingHasChanged = let saveHash' = saveHash sc latestHashedResponse
                                                    in when hashesAreDifferent saveHash' 
                let executeCallbackConfig' = executeCallbackConfig (sc ^. scrapeInfoCallbackConfig) response'
                (_, t) <- Async.concurrently saveHashIfSomethingHasChanged executeCallbackConfig'
                return t

repeatScrape :: CronSchedule -> ScrapeConfig () -> Scraper -> Either [ValidationError] (IO ())
repeatScrape cs sc s = 
  let cronSchedule = validateCronSchedule cs
      scrapeResult = scrape sc s ^. Validation._AccValidation
      scrapeResultRepeated = const repeatScrape' <$> cronSchedule <*> scrapeResult
  in scrapeResultRepeated ^. Validation._Either
  where repeatScrape' :: IO () -> IO ()
        repeatScrape' scrapeAction = () <$ CronSchedule.execSchedule (CronSchedule.addJob scrapeAction cs)

scrapeAll :: [(ScrapeConfig t, Scraper)] -> [(Url, Either [ValidationError] (IO ()))]
scrapeAll infos = let responses = TU.uncurry scrape <$> infos 
                      urls = (^. scrapeInfoUrl) <$> (fst <$> infos)
                  in urls `zip` responses
