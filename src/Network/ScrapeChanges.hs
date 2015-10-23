module Network.ScrapeChanges(
  scrape
, scrapeAll
, ScrapeInfo(..)
, defaultScrapeInfo
) where
import Network.ScrapeChanges.Internal
import qualified Data.Traversable as T
import qualified Data.Tuple as TU
import Control.Lens

type Error = String
type Response = String
type Url = String
type ScrapeInfoParser = String -> String

scrape :: ScrapeInfo t -> ScrapeInfoParser -> IO (Either Error Response)
scrape = undefined

scrapeAll :: [(ScrapeInfo t, ScrapeInfoParser)] -> IO [(Url, Either Error Response)]
scrapeAll infos = let responses = T.sequence $ TU.uncurry scrape <$> infos --maybe parallelize using parallel-io
                      urls = (^. scrapeInfoUrl) <$> (fst <$> infos)
                  in (urls `zip`) <$> responses
