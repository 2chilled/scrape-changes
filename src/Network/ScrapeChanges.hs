module Network.ScrapeChanges(
  scrape
, repeatScrape
, scrapeAll
, ScrapeConfig(..)
, defaultScrapeConfig
) where
import Network.ScrapeChanges.Internal as Internal
import Network.ScrapeChanges.Internal.Domain as Domain
import qualified Data.Validation as Validation
import qualified Data.Tuple as TU
import qualified System.Cron.Schedule as CronSchedule
import Control.Lens

type Url = String
type Scraper = String -> String

{-
- TODO
- 1. Validate scrapeInfo
- 2. Make http request
- 3. Hash http request and compare with latest hash
- 4. If hashes differ, execute callback config
-}
scrape :: ScrapeConfig t -> Scraper -> Either [ValidationError] (IO ())
scrape = undefined

repeatScrape :: CronSchedule -> ScrapeConfig t -> Scraper -> Either [ValidationError] (IO ())
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
