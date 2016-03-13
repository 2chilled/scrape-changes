{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (isInfixOf)
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Text.HTML.TagSoup (Tag(..), (~==), (~/=), parseTags, partitions, innerText, fromAttrib) 
import Data.Text.Lazy.Encoding (decodeUtf8With)

main :: IO ()
main = do 
  byteString <- B.readFile "/tmp/google.co.uk"
  let scrapeText = scrapeGoogleLogo byteString
  print scrapeText
  putStrLn "scrape-changes examples executable. Just look at the example source code."

-- |Google logo scrape job using the tagsoup library
scrapeGoogleLogo :: ByteString -> Text
scrapeGoogleLogo byteString =   
  let tags      = parseTags byteString
      filtered  = partitions (const True) $ -- (~== TagOpen "style" []) $
                  takeWhile (~/= TagClose ("style" :: ByteString)) $
                  dropWhile isDivWithBackgroundUrl tags 
  in T.unlines $ decodeUtf8Lenient . innerText <$> filtered 
  where decodeUtf8Lenient = decodeUtf8With $ const . const . Just $ '?'
        isDivWithBackgroundUrl :: Tag ByteString -> Bool
        isDivWithBackgroundUrl t = 
          let containsBackgroundUrl = isInfixOf "background:url" . toStrict
          in t ~== TagOpen ("div" :: ByteString) [] && containsBackgroundUrl (fromAttrib "style" t)
