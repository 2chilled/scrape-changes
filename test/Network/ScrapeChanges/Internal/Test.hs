module Network.ScrapeChanges.Internal.Test where
  import Network.ScrapeChanges.Internal as SUT
  import qualified Network.ScrapeChanges as SC
  import qualified Data.Maybe as M
  import Test.HUnit
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Control.Lens
  import Data.Validation
  import qualified Data.Validation as V

  tMailAddr = MailAddr (Just "Max Mustermann") "max@mustermann.com"

  correctUrl = "http://www.google.de"

  correctMailScrapeInfo :: ScrapeInfo t
  correctMailScrapeInfo = let setUrl = scrapeInfoUrl .~ correctUrl
                              setMailFrom = scrapeInfoCallbackConfig . _MailConfig . mailFrom .~ [tMailAddr]
                          in setUrl . setMailFrom $ SC.defaultScrapeInfo

  correctOtherScrapeInfo :: ScrapeInfo ()
  correctOtherScrapeInfo = let setCallbackConfig = scrapeInfoCallbackConfig .~ OtherConfig (const $ return ())
                           in setCallbackConfig SC.defaultScrapeInfo

  validateScrapeInfoWithBadInfoUrlShouldNotValidate :: Assertion
  validateScrapeInfoWithBadInfoUrlShouldNotValidate = 
      let wrongUrl = "httpp://www.google.de"
          scrapeInfo = scrapeInfoUrl .~ wrongUrl $ correctMailScrapeInfo 
          result = SUT.validateScrapeInfo scrapeInfo
      in  V.AccFailure [UrlProtocolInvalid] @=? result

  validateScrapeInfoWithBadMailFromShouldNotValidate :: Assertion
  validateScrapeInfoWithBadMailFromShouldNotValidate = 
      let scrapeInfo = (scrapeInfoCallbackConfig . _MailConfig . mailFrom) .~ [] $ correctMailScrapeInfo 
          result = SUT.validateScrapeInfo scrapeInfo
      in  V.AccFailure [SUT.MailConfigEmptyFrom] @=? result

  validateScrapeInfoShouldValidateOnValidInput :: Assertion
  validateScrapeInfoShouldValidateOnValidInput =
      let scrapeInfo = SUT.scrapeInfoUrl .~ correctUrl $ correctMailScrapeInfo 
          result = SUT.validateScrapeInfo scrapeInfo
      in  V.AccSuccess scrapeInfo @=? result

  tests = 
    [
      testGroup "Network.ScrapeChanges.Internal"
      [
        testCase "validateScrapeInfo with bad info url should not validate"
          validateScrapeInfoWithBadInfoUrlShouldNotValidate
      , testCase "validateScrapeInfo should validate on valid input"
          validateScrapeInfoShouldValidateOnValidInput
      , testCase "validateScrapeInfo with bad mail from should not validate"
          validateScrapeInfoWithBadMailFromShouldNotValidate
      ]
    ]

