{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (isInfixOf)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Text.HTML.TagSoup (Tag(..), (~==), (~/=), parseTags, fromAttrib) 
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified System.Log.Logger as Logger
import qualified System.Log.Handler.Syslog as Syslog
import Data.Monoid ((<>))
import Control.Monad (forever)
import Network.ScrapeChanges

main :: IO ()
main = do
  _ <- configureLogging
  _ <- (Logger.errorM thisLogger . show) `either` id $ scrapeChangesJobs 
  putStrLn "scrape-changes examples executable. Just look at the example source code."
  -- |Simplest way to block the main thread forever. Good enough for the use cases of 'scrape-changes'
  _ <- forever getLine
  -- |Will never be executed in this case
  clearAllScrapeConfigs

-- |Google logo scrape function using the tagsoup library
scrapeGoogleLogo :: ByteString -> Text
scrapeGoogleLogo byteString =   
  let tags                 = parseTags byteString
      divWithBackgroundUrl = find (~/= TagClose ("div" :: ByteString)) $
                             dropWhile (not . isDivWithBackgroundUrl) tags 
      resultMaybe          = decodeUtf8Lenient . styleAttribContent <$> divWithBackgroundUrl
  in fromMaybe "" resultMaybe 
  where decodeUtf8Lenient = decodeUtf8With $ const . const . Just $ '?'
        isDivWithBackgroundUrl t = 
          let containsBackgroundUrl = isInfixOf "background:url" . toStrict
          in t ~== TagOpen ("div" :: ByteString) [] && containsBackgroundUrl (styleAttribContent t)
        styleAttribContent = fromAttrib "style"

scrapeChangesJobs :: Either [(Url, [ValidationError])] (IO ())
scrapeChangesJobs = repeatScrapeAll [
    -- Checks each minute for changes and sends a mail if there are any
    ScrapeSchedule {
      _scrapeScheduleCron = "* * * * *" -- std cron format
    , _scrapeScheduleConfig = mailScrapeConfig "http://www.google.co.uk" -- to scrape
                                               (MailAddr Nothing "max@mustermann.de") -- from
                                               (MailAddr Nothing "receiver@scrape-changes.com" :| []) -- to
    , _scrapeScheduleScraper = scrapeGoogleLogo --scrape function
    }
    -- Checks each minute for changes and notifies to syslog if there are any
  , ScrapeSchedule {
      _scrapeScheduleCron = "* * * * *"
    , _scrapeScheduleConfig = otherScrapeConfig "http://www.google.co.uk" 
                                                (\text -> Logger.infoM thisLogger . show $ 
                                                          "Change detected: " <> text)
    , _scrapeScheduleScraper = scrapeGoogleLogo
    }
  ]

configureLogging :: IO ()
configureLogging = do
  syslogHandler <- Syslog.openlog thisLogger [] Syslog.DAEMON Logger.DEBUG
  let logConfig = flip Logger.updateGlobalLogger (Logger.addHandler syslogHandler . Logger.setLevel Logger.DEBUG)
  sequence_ $ logConfig <$> ["Network.ScrapeChanges", thisLogger]

thisLogger :: String
thisLogger = "scrape-changes-examples"
