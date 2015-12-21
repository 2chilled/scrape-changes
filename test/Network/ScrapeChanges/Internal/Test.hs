{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.ScrapeChanges.Internal.Test where
import Prelude hiding (filter)
import Network.ScrapeChanges.Internal as SUT
import Network.ScrapeChanges.Internal.Domain as Domain
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
import qualified Data.Hashable as Hashable

newtype NCronSchedule = NCronSchedule { nCronScheduleRun :: String } deriving Show

instance Arbitrary NCronSchedule where 
    arbitrary = NCronSchedule <$> oneof [pure correctCronSchedule, arbitrary]

correctCronSchedule :: String
correctCronSchedule = "*/2 * 3 * 4,5,6"

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

instance Arbitrary (ScrapeConfig ()) where
  arbitrary = helper <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    where helper scrapeInfoUrl' config' mailFromAddr mailToAddrs = 
            let setConfig = scrapeInfoCallbackConfig .~ config'
            in setConfig $ SC.mailScrapeConfig scrapeInfoUrl' mailFromAddr mailToAddrs

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary

tMailAddr :: MailAddr
tMailAddr = MailAddr (Just "Max Mustermann") "max@mustermann.com"

correctMailAddr :: String
correctMailAddr = "correct@mail.com"

correctUrl :: String
correctUrl = "http://www.google.de"

correctMailScrapeConfig :: ScrapeConfig t
correctMailScrapeConfig = SC.mailScrapeConfig correctUrl tMailAddr (tMailAddr :| [])

correctOtherScrapeConfig :: ScrapeConfig ()
correctOtherScrapeConfig = let setCallbackConfig = scrapeInfoCallbackConfig .~ OtherConfig (const $ return ())
                           in setCallbackConfig correctMailScrapeConfig

validateScrapeConfigWithBadInfoUrlShouldNotValidate :: Assertion
validateScrapeConfigWithBadInfoUrlShouldNotValidate = 
    let wrongUrl = "httpp://www.google.de"
        scrapeInfo = scrapeInfoUrl .~ wrongUrl $ correctMailScrapeConfig 
        result = SUT.validateScrapeConfig scrapeInfo
    in  V.AccFailure [UrlProtocolInvalid] @=? result

validateScrapeConfigShouldValidateOnValidInput :: Assertion
validateScrapeConfigShouldValidateOnValidInput =
    let scrapeInfo = scrapeInfoUrl .~ correctUrl $ correctMailScrapeConfig 
        result = SUT.validateScrapeConfig scrapeInfo
    in  V.AccSuccess scrapeInfo @=? result

validateScrapeConfigWithOtherConfigShouldSatisfyAllInvariants :: ScrapeConfig () -> Property
validateScrapeConfigWithOtherConfigShouldSatisfyAllInvariants si = M.isJust (si ^? scrapeInfoCallbackConfig . _OtherConfig) ==>
  let result = SUT.validateScrapeConfig si
      p1 = property $ M.isJust (result ^? V._Success)
      badUrlErrorsOnly = (null . (L.\\ [UrlNotAbsolute, UrlProtocolInvalid])) <$> (result ^? V._Failure)
      p2 = property $ False `M.fromMaybe` badUrlErrorsOnly
  in p1 .||. p2
  
validateScrapeConfigWithMailConfigShouldSatisfyAllInvariants :: ScrapeConfig () -> Property
validateScrapeConfigWithMailConfigShouldSatisfyAllInvariants si = M.isJust (si ^? scrapeInfoCallbackConfig . _MailConfig) ==>
  let result = SUT.validateScrapeConfig si
      failure = result ^? V._Failure
      mailConfigLens = scrapeInfoCallbackConfig . _MailConfig
      (Just mailConfig) = si ^? mailConfigLens
      invalidMailAddr = not . EmailValidate.isValid . (^. ByteStringLens.packedChars)
      invalidMailAddrsProp es = let expected = (\errors' -> (`elem` errors') `L.all` es) <$> failure 
                                in True `M.fromMaybe` expected
      mailFromAddr = mailConfig ^. mailFrom . mailAddr
      mailToAddrs = mailConfig ^.. mailTo . traverse . mailAddr
      invalidMailFromAddrsProp = invalidMailAddrsProp $ MailConfigInvalidMailFromAddr <$> [mailFromAddr | invalidMailAddr mailFromAddr]
      invalidMailToAddrs = invalidMailAddr `L.filter` mailToAddrs
      invalidMailToAddrsProp = invalidMailAddrsProp $ MailConfigInvalidMailToAddr <$> invalidMailToAddrs
  in property invalidMailFromAddrsProp .&&. property invalidMailToAddrsProp

validateCronScheduleShouldSatisfyAllInvariants :: NCronSchedule -> Property
validateCronScheduleShouldSatisfyAllInvariants c = 
  let result = SUT.validateCronSchedule $ nCronScheduleRun c
      isCorrect = nCronScheduleRun c /= correctCronSchedule
                    || M.isJust (result ^? V._Success)
      containsExpectedError = False `M.fromMaybe` ((CronScheduleInvalid "" `elem`) <$> (result ^? V._Failure))
  in property isCorrect .||. property containsExpectedError

differentScrapeConfigsShouldYieldToDifferentHashes :: ScrapeConfig () -> ScrapeConfig () -> Property
differentScrapeConfigsShouldYieldToDifferentHashes c1 c2 = 
  let isOtherConfig = M.isJust . (^? scrapeInfoCallbackConfig . _OtherConfig)
  in not (isOtherConfig c1 && isOtherConfig c2) ==> 
    let hashedC1 = Hashable.hash c1
        hashedC2 = Hashable.hash c2
        correspondenceBetweenEqualsAndHashable = 
          label "correspondenceBetweenEqualsAndHashable" $ if c1 == c2 then hashedC1 == hashedC2
                                                                       else hashedC1 /= hashedC2
        differentMailConfigAttribute = let setMailSubject = set $ scrapeInfoCallbackConfig . _MailConfig . mailSubject
                                           c1' = setMailSubject "sub1" c1
                                           c2' = setMailSubject "sub2" c2
                                       in label "differentMailConfigAttribute" $ c1' /= c2'
    in correspondenceBetweenEqualsAndHashable .&&. differentMailConfigAttribute

tests :: [TF.Test]
tests = 
  [
    testGroup "Network.ScrapeChanges.Internal"
    [
      testCase "validateScrapeConfig with bad info url should not validate"
        validateScrapeConfigWithBadInfoUrlShouldNotValidate
    , testCase "validateScrapeConfig should validate on valid input"
        validateScrapeConfigShouldValidateOnValidInput
    , testProperty "validateScrapeConfig with mail config should satisfy all invariants"
        validateScrapeConfigWithMailConfigShouldSatisfyAllInvariants
    , testProperty "validateScrapeConfig with other config should satisfy all invariants"
        validateScrapeConfigWithOtherConfigShouldSatisfyAllInvariants
    , testProperty "validateCronSchedule should satisfy all invariants"
        validateCronScheduleShouldSatisfyAllInvariants
    , testProperty "Different ScrapeConfig's should yield to different hashes"
        differentScrapeConfigsShouldYieldToDifferentHashes
    ]
  ]

