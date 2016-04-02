# scrape-changes
This library scrapes websites and invokes callbacks when there are changes, similar to a RSS reader.
Sending an email by invoking sendmail is a built-in callback mechanism. Of course, users can provide 
their own callback function in addition.

If you need more control over the scraping process (e.g. for using another HTTP method than GET), just open an
issue or file a PR.

## Example usage
```haskell
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

runJobs :: IO ()
runJobs = (Logger.errorM thisLogger . show) `either` id $ scrapeChangesJobs 
```
Please look at **scrape-changes-examples/examples.hs** for a full working example
