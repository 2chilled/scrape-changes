module Network.ScrapeChanges(
  scrape
, repeatScrape
, scrapeAll
, ScrapeConfig(..)
, defaultScrapeConfig
) where
import Network.ScrapeChanges.Internal as Internal
import Network.ScrapeChanges.Internal.Domain as Domain
import qualified Data.Tuple as TU
import Control.Lens

type Url = String
type Scraper = String -> String

{-
- TODO
- 1. Validate scrapeInfo
- 2. Maybe execute scrapeInfo
-}
scrape :: ScrapeConfig t -> Scraper -> Either [Domain.ValidationError] (IO ())
scrape = undefined

repeatScrape :: CronSchedule -> ScrapeConfig t -> Scraper -> Either [Domain.ValidationError] (IO ())
repeatScrape = undefined

scrapeAll :: [(ScrapeConfig t, Scraper)] -> [(Url, Either [Domain.ValidationError] (IO ()))]
scrapeAll infos = let responses = TU.uncurry scrape <$> infos 
                      urls = (^. scrapeInfoUrl) <$> (fst <$> infos)
                  in urls `zip` responses
