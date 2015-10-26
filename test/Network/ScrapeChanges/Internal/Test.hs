{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.ScrapeChanges.Internal.Test where
import Network.ScrapeChanges.Internal as SUT
import qualified Network.ScrapeChanges as SC
import qualified Data.Maybe as M
import qualified Data.List as L
import Test.HUnit hiding (assertFailure)
import Test.QuickCheck
import Test.Framework as TF
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Control.Lens
import qualified Data.ByteString.Lens as ByteStringLens
import qualified Data.Validation as V
import qualified Text.Email.Validate as EmailValidate

emailAddressGen :: Gen String
emailAddressGen = oneof [pure correctMailAddr, arbitrary]

instance Arbitrary MailAddr where
  arbitrary = MailAddr <$> arbitrary <*> emailAddressGen

instance Arbitrary Mail where
  arbitrary = Mail <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (CallbackConfig ()) where
  arbitrary = oneof [otherConfigGen, mailConfigGen]
    where otherConfigGen = return $ OtherConfig (const $ return ())
          mailConfigGen = MailConfig <$> arbitrary

instance Arbitrary (ScrapeInfo ()) where
  arbitrary = do
    scrapeInfoUrl' <- arbitrary
    config' <- arbitrary
    let setUrl = scrapeInfoUrl .~ scrapeInfoUrl'
    let setConfig = scrapeInfoCallbackConfig .~ config'
    return $ setUrl . setConfig $ SC.defaultScrapeInfo

tMailAddr :: MailAddr
tMailAddr = MailAddr (Just "Max Mustermann") "max@mustermann.com"

correctMailAddr :: String
correctMailAddr = "correct@mail.com"

correctUrl :: String
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

validateScrapeInfoShouldSatisfyAllInvariants :: ScrapeInfo () -> Property
validateScrapeInfoShouldSatisfyAllInvariants si =
  let result = SUT.validateScrapeInfo si
      success = result ^? V._Success
      failure = result ^? V._Failure
      otherConfigLens = scrapeInfoCallbackConfig . _OtherConfig
      mailConfigLens = scrapeInfoCallbackConfig . _MailConfig
  in case si of _ | M.isJust (si ^? otherConfigLens) ->
                    let p1 = label "Validation successful" (M.isJust success)
                        badUrlErrorsOnly = ((null . (L.\\ [UrlNotAbsolute, UrlProtocolInvalid])) <$> failure)
                        p2 = label "Validation unsuccessful because of bad url " (False `M.fromMaybe` badUrlErrorsOnly)
                    in p1 .||. p2
                  | M.isJust (si ^? mailConfigLens) -> 
                    let mailConfig = undefined `M.fromMaybe` (si ^? mailConfigLens)
                        mailFrom' = mailConfig ^. mailFrom
                        emptyMailFromProp = (not . null $ mailFrom') 
                                              || (MailConfigEmptyFrom `elem` (L.concat . M.maybeToList $ failure))
                        invalidMailFromAddrs = let mailAddrs = mailConfig ^.. (mailFrom . traverse . mailAddr)
                                                   f = not . EmailValidate.isValid . (^. ByteStringLens.packedChars)
                                               in f `filter` mailAddrs
                        invalidMailFromAddrsProp = let mapped' = MailConfigInvalidMailFromAddr <$> invalidMailFromAddrs
                                                       expected = (\errors' -> (`elem` errors') `L.all` mapped') <$> failure 
                                                   in True `M.fromMaybe` expected
                    in property emptyMailFromProp .&. property invalidMailFromAddrsProp
                  | otherwise -> undefined

tests :: [TF.Test]
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
    , testProperty "validateScrapeInfo should satisfy all invariants"
        validateScrapeInfoShouldSatisfyAllInvariants
    ]
  ]

