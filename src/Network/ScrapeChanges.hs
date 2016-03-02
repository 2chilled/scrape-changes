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
, mailScrapeConfig
, otherScrapeConfig
, clearScrapeConfig
, clearAllScrapeConfigs
, module Domain
) where

import Network.ScrapeChanges.Internal as Internal
import Network.ScrapeChanges.Domain as Domain
import qualified Data.Validation as Validation
import qualified Data.Tuple as TU
import qualified System.Cron.Schedule as CronSchedule
import Control.Lens
import qualified Network.Wreq as Http
import qualified Control.Concurrent.Async as Async
import qualified System.Log.Logger as Log
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Data.Traversable as Traversable
import qualified Control.Monad as Monad
import qualified Control.Exception as Exception

-- |The basic scrape function. It fires a GET request against the url defined within the provided
-- 'ScrapeConfig'. The body is passed to the provided 'Scraper'. The result 'Data.Text.Lazy.Text' of the
-- latter is used to determine whether something has changed on the respective website. If so, the callback
-- configured in 'ScrapeConfig' is executed and 'CallbackCalled' is returned. Otherwise 'CallbackNotCalled' is
-- returned.
scrape :: ScrapeConfig -> Scraper -> Either [ValidationError] (IO ScrapeResult)
scrape sc s = let result = scrapeOrchestration <$ validateScrapeConfig sc
              in result ^. Validation._Either
  where scrapeOrchestration = 
          let responseBody = (^. Http.responseBody)
              urlToRequest = sc ^. scrapeInfoUrl
              requestLog = Log.infoM loggerName $ "Requesting " ++ urlToRequest
              request = (s . responseBody <$>) . Http.get
              response = (request urlToRequest <* requestLog) `Exception.catch` httpExceptionHandler sc
          in do (response', latestHashedResponse) <- Async.concurrently response (readLatestHash sc)
                let currentHashedResponse = hash' response'
                let executeCallbackConfig' = executeCallbackConfig sc response'
                let saveHash'' = saveHash' currentHashedResponse urlToRequest
                let saveHashAndExecuteCallbackConfig = Async.concurrently  saveHash'' executeCallbackConfig'

                if hashesAreDifferent latestHashedResponse currentHashedResponse 
                then CallbackCalled <$ saveHashAndExecuteCallbackConfig 
                else pure CallbackNotCalled 

        hashesAreDifferent :: LatestHash -> CurrentHash -> Bool
        hashesAreDifferent latestHash currentHash = 
          Maybe.isNothing latestHash || Foldable.or ((/= currentHash) <$> latestHash) 

        saveHash' :: Hash -> Url -> IO ()
        saveHash' h url = let saveHashMsg = "Saved new hash for url '" ++ url ++ "'"
                              saveHashLog = Log.infoM loggerName saveHashMsg
                          in  saveHash sc h <* saveHashLog

type LatestHash = Maybe String
type CurrentHash = String

-- |Repeat executing 'scrape' by providing a 'CronScheduleString'. The returned
-- IO action is non blocking
repeatScrape :: CronScheduleString -> ScrapeConfig -> Scraper -> Either [ValidationError] (IO ())
repeatScrape cs sc s = let result = repeatScrapeAll [ScrapeSchedule cs sc s]
                           resultErrorMapped = (snd . head <$> (result ^. swapped)) ^. swapped
                       in resultErrorMapped

-- |Execute a list of 'ScrapeSchedule' in parallel. If validation of any 'ScrapeSchedule' fails, 
-- 'Left' containing 'ValidationError' indexed by the corresponding 'Url' is returned.
repeatScrapeAll :: [ScrapeSchedule] -> Either [(Url, [ValidationError])] (IO ())
repeatScrapeAll scrapeSchedules = 
  let cronSchedules = Traversable.for scrapeSchedules $ \(ScrapeSchedule cronSchedule scrapeConfig scraper) ->
        let scrapeConfigUrl = scrapeConfig ^. scrapeInfoUrl
            cronScheduleValidation = validateCronSchedule cronSchedule
            resultValidation = scrape scrapeConfig scraper ^. Validation._AccValidation
            result = toCronSchedule <$> resultValidation <*> cronScheduleValidation
        in  ((\x -> [(scrapeConfigUrl, x)]) <$> (result ^. swapped)) ^. swapped
  in (Monad.void . CronSchedule.execSchedule . Foldable.sequenceA_) <$> cronSchedules ^. Validation._Either
  where toCronSchedule :: IO t -> CronScheduleString -> CronSchedule.Schedule ()
        toCronSchedule scrapeAction = CronSchedule.addJob (Monad.void scrapeAction) 
          
-- |Execute a list of 'ScrapeConfig' in sequence using 'scrape' and collect
-- the results in a map containing the respective 'Url' as key.
scrapeAll :: [(ScrapeConfig, Scraper)] -> [(Url, Either [ValidationError] (IO ScrapeResult))]
scrapeAll infos = let responses = TU.uncurry scrape <$> infos 
                      urls = (^. scrapeInfoUrl) <$> (fst <$> infos)
                  in urls `zip` responses

-- |Clear all mutable state associated with the provided 'ScrapeConfig'
clearScrapeConfig :: ScrapeConfig -> IO ()
clearScrapeConfig = removeHash

-- |Clear all mutable state ever used by 'scrape-changes'
clearAllScrapeConfigs :: IO ()
clearAllScrapeConfigs = removeHashes
