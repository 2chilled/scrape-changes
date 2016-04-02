-- |
-- Module      : Network.ScrapeChanges 
-- Copyright   : (C) 2015-16 Matthias Herrmann
-- License     : GPL-3
-- Maintainer  : matthias.mh.herrmann@gmail.com
--
-- Main module, reexports everything you need to use "scrape-changes". Full working example:
--
-- @
--
-- {-\# LANGUAGE OverloadedStrings \#-}
-- 
-- import Data.ByteString (isInfixOf)
-- import Data.ByteString.Lazy (ByteString, toStrict)
-- import Data.Text.Lazy.Encoding (decodeUtf8With)
-- import Data.Foldable (find)
-- import Data.Maybe (fromMaybe)
-- import Text.HTML.TagSoup (Tag(..), (~==), (~/=), parseTags, fromAttrib) 
-- import Data.List.NonEmpty (NonEmpty ((:|)))
-- import qualified System.Log.Logger as Logger
-- import qualified System.Log.Handler.Syslog as Syslog
-- import Data.Monoid ((<>))
-- import Control.Monad (forever)
-- import Network.ScrapeChanges
-- 
-- main :: IO ()
-- main = do
--   _ <- configureLogging
--   _ <- (Logger.errorM thisLogger . show) `either` id $ scrapeChangesJobs 
--   putStrLn "scrape-changes examples executable. Just look at the example source code."
--   -- |Simplest way to block the main thread forever. Good enough for the use cases of 'scrape-changes'
--   _ <- forever getLine
--   -- |Will never be executed in this case
--   clearAllScrapeConfigs
-- 
-- -- |Google logo scrape function using the tagsoup library
-- scrapeGoogleLogo :: ByteString -> Text
-- scrapeGoogleLogo byteString =   
--   let tags                 = parseTags byteString
--       divWithBackgroundUrl = find (~/= TagClose ("div" :: ByteString)) $
--                              dropWhile (not . isDivWithBackgroundUrl) tags 
--       resultMaybe          = decodeUtf8Lenient . styleAttribContent <$> divWithBackgroundUrl
--   in fromMaybe "" resultMaybe 
--   where decodeUtf8Lenient = decodeUtf8With $ const . const . Just $ '?'
--         isDivWithBackgroundUrl t = 
--           let containsBackgroundUrl = isInfixOf "background:url" . toStrict
--           in t ~== TagOpen ("div" :: ByteString) [] && containsBackgroundUrl (styleAttribContent t)
--         styleAttribContent = fromAttrib "style"
-- 
-- scrapeChangesJobs :: Either [(Url, [ValidationError])] (IO ())
-- scrapeChangesJobs = repeatScrapeAll [
--     -- Checks each minute for changes and sends a mail if there are any
--     ScrapeSchedule {
--       _scrapeScheduleCron = "* * * * *" -- std cron format
--     , _scrapeScheduleConfig = mailScrapeConfig "http://www.google.co.uk" -- to scrape
--                                                (MailAddr Nothing "max@mustermann.de") -- from
--                                                (MailAddr Nothing "receiver@scrape-changes.com" :| []) -- to
--     , _scrapeScheduleScraper = scrapeGoogleLogo --scrape function
--     }
--     -- Checks each minute for changes and notifies to syslog if there are any
--   , ScrapeSchedule {
--       _scrapeScheduleCron = "* * * * *"
--     , _scrapeScheduleConfig = otherScrapeConfig "http://www.google.co.uk" 
--                                                 (\text -> Logger.infoM thisLogger . show $ 
--                                                           "Change detected: " <> text)
--     , _scrapeScheduleScraper = scrapeGoogleLogo
--     }
--   ]
-- 
-- configureLogging :: IO ()
-- configureLogging = do
--   syslogHandler <- Syslog.openlog thisLogger [] Syslog.DAEMON Logger.DEBUG
--   let logConfig = flip Logger.updateGlobalLogger (Logger.addHandler syslogHandler . Logger.setLevel Logger.DEBUG)
--   sequence_ $ logConfig <$> ["Network.ScrapeChanges", thisLogger]
-- 
-- thisLogger :: String
-- thisLogger = "scrape-changes-examples"
-- @
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

-- |Clear all mutable state ever used by "scrape-changes"
clearAllScrapeConfigs :: IO ()
clearAllScrapeConfigs = removeHashes
