{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (isInfixOf)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import qualified Data.Text.Lazy.IO as TextIO
import Data.Text.Lazy (append) 
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Text.HTML.TagSoup (Tag(..), (~==), (~/=), parseTags, fromAttrib) 
import Data.List.NonEmpty (NonEmpty ((:|)))
import Network.ScrapeChanges

main :: IO ()
main = putStrLn "scrape-changes examples executable. Just look at the example source code."

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
    -- |Checks each minute for changes and sends a mail if there are any
    ScrapeSchedule {
      _scrapeScheduleCron = "* * * * *"
    , _scrapeScheduleConfig = mailScrapeConfig "http://www.google.co.uk" 
                                               (MailAddr Nothing "max@mustermann.de") --from
                                               (MailAddr Nothing "receiver@scrape-changes.com" :| []) --to
    , _scrapeScheduleScraper = scrapeGoogleLogo
    }
    -- |Checks each minute for changes and notifies stdout if there are any
  , ScrapeSchedule {
      _scrapeScheduleCron = "* * * * *"
    , _scrapeScheduleConfig = otherScrapeConfig "http://www.google.co.uk" 
                                                (\text -> TextIO.putStrLn $ "Change detected: " `append` text)
    , _scrapeScheduleScraper = scrapeGoogleLogo
    }
  ]
