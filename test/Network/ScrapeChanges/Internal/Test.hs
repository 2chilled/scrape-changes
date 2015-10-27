{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.ScrapeChanges.Internal.Test where
import Prelude hiding (filter)
import Network.ScrapeChanges.Internal as SUT
import qualified Network.ScrapeChanges as SC
import qualified Data.Maybe as M
import qualified Data.List as L
import Data.List.NonEmpty
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

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary

tMailAddr :: MailAddr
tMailAddr = MailAddr (Just "Max Mustermann") "max@mustermann.com"

correctMailAddr :: String
correctMailAddr = "correct@mail.com"

correctUrl :: String
correctUrl = "http://www.google.de"

correctMailScrapeInfo :: ScrapeInfo t
correctMailScrapeInfo = let setUrl = scrapeInfoUrl .~ correctUrl
                            setMail x = scrapeInfoCallbackConfig . _MailConfig . x .~ (tMailAddr :| [])
                            setMailFrom = setMail mailFrom
                            setMailTo = setMail mailTo 
                        in setUrl . setMailFrom . setMailTo $ SC.defaultScrapeInfo

correctOtherScrapeInfo :: ScrapeInfo ()
correctOtherScrapeInfo = let setCallbackConfig = scrapeInfoCallbackConfig .~ OtherConfig (const $ return ())
                         in setCallbackConfig SC.defaultScrapeInfo

validateScrapeInfoWithBadInfoUrlShouldNotValidate :: Assertion
validateScrapeInfoWithBadInfoUrlShouldNotValidate = 
    let wrongUrl = "httpp://www.google.de"
        scrapeInfo = scrapeInfoUrl .~ wrongUrl $ correctMailScrapeInfo 
        result = SUT.validateScrapeInfo scrapeInfo
    in  V.AccFailure [UrlProtocolInvalid] @=? result

validateScrapeInfoShouldValidateOnValidInput :: Assertion
validateScrapeInfoShouldValidateOnValidInput =
    let scrapeInfo = SUT.scrapeInfoUrl .~ correctUrl $ correctMailScrapeInfo 
        result = SUT.validateScrapeInfo scrapeInfo
    in  V.AccSuccess scrapeInfo @=? result

validateScrapeInfoWithOtherConfigShouldSatisfyAllInvariants :: ScrapeInfo () -> Property
validateScrapeInfoWithOtherConfigShouldSatisfyAllInvariants si = M.isJust (si ^? scrapeInfoCallbackConfig . _OtherConfig) ==>
  let result = SUT.validateScrapeInfo si
      p1 = property $ M.isJust (result ^? V._Success)
      badUrlErrorsOnly = (null . (L.\\ [UrlNotAbsolute, UrlProtocolInvalid])) <$> (result ^? V._Failure)
      p2 = property $ False `M.fromMaybe` badUrlErrorsOnly
  in p1 .||. p2
  

validateScrapeInfoWithMailConfigShouldSatisfyAllInvariants :: ScrapeInfo () -> Property
validateScrapeInfoWithMailConfigShouldSatisfyAllInvariants si = M.isJust (si ^? scrapeInfoCallbackConfig . _MailConfig) ==>
  let result = SUT.validateScrapeInfo si
      failure = result ^? V._Failure
      mailConfigLens = scrapeInfoCallbackConfig . _MailConfig
      (Just mailConfig) = si ^? mailConfigLens
      invalidMailAddrs t = let mailAddrs = mailConfig ^.. (t . traverse . mailAddr)
                               f = not . EmailValidate.isValid . (^. ByteStringLens.packedChars)
                           in f `L.filter` mailAddrs
      invalidMailAddrsProp es = let expected = (\errors' -> (`elem` errors') `L.all` es) <$> failure 
                                in True `M.fromMaybe` expected
      invalidMailFromAddrs = invalidMailAddrs mailFrom
      invalidMailFromAddrsProp = invalidMailAddrsProp $ MailConfigInvalidMailFromAddr <$> invalidMailFromAddrs
      invalidMailToAddrs = invalidMailAddrs mailTo
      invalidMailToAddrsProp = invalidMailAddrsProp $ MailConfigInvalidMailToAddr <$> invalidMailToAddrs
  in property invalidMailFromAddrsProp .&&. property invalidMailToAddrsProp

tests :: [TF.Test]
tests = 
  [
    testGroup "Network.ScrapeChanges.Internal"
    [
      testCase "validateScrapeInfo with bad info url should not validate"
        validateScrapeInfoWithBadInfoUrlShouldNotValidate
    , testCase "validateScrapeInfo should validate on valid input"
        validateScrapeInfoShouldValidateOnValidInput
    , testProperty "validateScrapeInfo with mail config should satisfy all invariants"
        validateScrapeInfoWithMailConfigShouldSatisfyAllInvariants
    , testProperty "validateScrapeInfo with other config should satisfy all invariants"
        validateScrapeInfoWithOtherConfigShouldSatisfyAllInvariants
    ]
  ]

