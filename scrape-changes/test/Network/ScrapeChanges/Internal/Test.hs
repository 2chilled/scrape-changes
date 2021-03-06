{-# LANGUAGE FlexibleInstances, QuasiQuotes, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.ScrapeChanges.Internal.Test where
import Prelude hiding (filter)
import Network.ScrapeChanges.Internal as SUT
import Network.ScrapeChanges.Domain as Domain
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
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Hashable as Hashable
import Text.Shakespeare.Text

newtype NCronScheduleString = NCronScheduleString { nCronScheduleStringRun :: String } deriving Show

instance Show ScrapeConfig where
  show (ScrapeConfig url (MailConfig mail))= show [lt|
      ScrapeConfig { _scrapeInfoUrl = #{url}
                   , _scrapeInfoCallbackConfig = MailConfig #{show mail}
                   }
    |]
  show (ScrapeConfig url (OtherConfig _))= show [lt|
      ScrapeConfig { _scrapeInfoUrl = #{url}
                   , _scrapeInfoCallbackConfig = OtherConfig (\x -> return ())
                   }
    |]

instance Arbitrary NCronScheduleString where
    arbitrary = NCronScheduleString <$> oneof [pure correctCronScheduleString, arbitrary]

correctCronScheduleString :: String
correctCronScheduleString = "*/2 * 3 * 4,5,6"

emailAddressGen :: Gen String
emailAddressGen = oneof [pure correctMailAddr, arbitrary]

instance Arbitrary MailAddr where
  arbitrary = MailAddr <$> arbitrary <*> emailAddressGen

instance Arbitrary TextLazy.Text where
  arbitrary = TextLazy.pack <$> arbitrary

instance Arbitrary Mail where
  arbitrary = Mail <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary CallbackConfig where
  arbitrary = oneof [otherConfigGen, mailConfigGen]
    where otherConfigGen = return $ OtherConfig (const $ return ())
          mailConfigGen = MailConfig <$> arbitrary

instance Arbitrary ScrapeConfig where
  arbitrary = helper <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    where helper scrapeInfoUrl' config' mailFromAddr mailToAddrs =
            let setConfig = scrapeInfoCallbackConfig .~ config'
            in setConfig $ SC.mailScrapeConfig scrapeInfoUrl' mailFromAddr mailToAddrs

tMailAddr :: MailAddr
tMailAddr = MailAddr (Just $ TextLazy.pack "Max Mustermann") "max@mustermann.com"

correctMailAddr :: String
correctMailAddr = "correct@mail.com"

correctUrl :: String
correctUrl = "http://www.google.de"

correctMailScrapeConfig :: ScrapeConfig
correctMailScrapeConfig = SC.mailScrapeConfig correctUrl tMailAddr (tMailAddr :| [])

correctOtherScrapeConfig :: ScrapeConfig
correctOtherScrapeConfig = let setCallbackConfig = scrapeInfoCallbackConfig .~ OtherConfig (const $ return ())
                           in setCallbackConfig correctMailScrapeConfig

validateScrapeConfigWithBadInfoUrlShouldNotValidate :: Assertion
validateScrapeConfigWithBadInfoUrlShouldNotValidate =
    let wrongUrl = "httpp://www.google.de"
        scrapeInfo = scrapeInfoUrl .~ wrongUrl $ correctMailScrapeConfig
        result = SUT.validateScrapeConfig scrapeInfo
    in  Just [UrlProtocolInvalid] @=? result ^? V._Failure

validateScrapeConfigShouldValidateOnValidInput :: Assertion
validateScrapeConfigShouldValidateOnValidInput =
    let scrapeInfo = scrapeInfoUrl .~ correctUrl $ correctMailScrapeConfig
        result = SUT.validateScrapeConfig scrapeInfo
    in  result @=? V.AccSuccess scrapeInfo

validateScrapeConfigWithOtherConfigShouldSatisfyAllInvariants :: ScrapeConfig -> Property
validateScrapeConfigWithOtherConfigShouldSatisfyAllInvariants si = M.isJust (si ^? scrapeInfoCallbackConfig . _OtherConfig) ==>
  let result = SUT.validateScrapeConfig si
      p1 = property $ M.isJust (result ^? V._Success)
      badUrlErrorsOnly = (null . (L.\\ [UrlNotAbsolute, UrlProtocolInvalid])) <$> (result ^? V._Failure)
      p2 = property $ False `M.fromMaybe` badUrlErrorsOnly
  in p1 .||. p2

validateScrapeConfigWithMailConfigShouldSatisfyAllInvariants :: ScrapeConfig -> Property
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

validateCronScheduleShouldSatisfyAllInvariants :: NCronScheduleString -> Property
validateCronScheduleShouldSatisfyAllInvariants c =
  let result = SUT.validateCronSchedule $ nCronScheduleStringRun c
      isCorrect = nCronScheduleStringRun c /= correctCronScheduleString
                    || M.isJust (result ^? V._Success)
      containsExpectedError = False `M.fromMaybe` ((CronScheduleInvalid "" `elem`) <$> (result ^? V._Failure))
  in property isCorrect .||. property containsExpectedError

differentScrapeConfigsShouldYieldToDifferentHashes :: ScrapeConfig -> ScrapeConfig -> Property
differentScrapeConfigsShouldYieldToDifferentHashes c1 c2 =
  let isOtherConfig = M.isJust . (^? scrapeInfoCallbackConfig . _OtherConfig)
  in not (isOtherConfig c1 && isOtherConfig c2) ==>
    let hashedC1 = Hashable.hash c1
        hashedC2 = Hashable.hash c2
        correspondenceBetweenEqualsAndHashable =
          label "correspondenceBetweenEqualsAndHashable" $ if c1 == c2 then hashedC1 == hashedC2
                                                                       else hashedC1 /= hashedC2
        differentMailConfigAttribute = let setMailSubject = set $ scrapeInfoCallbackConfig . _MailConfig . mailSubject
                                           c1' = c1 & setMailSubject (TextLazy.pack "sub1")
                                           c2' = c2 & setMailSubject (TextLazy.pack "sub2")
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
    , testProperty "validateCronScheduleString should satisfy all invariants"
        validateCronScheduleShouldSatisfyAllInvariants
    , testProperty "Different ScrapeConfig's should yield to different hashes"
        differentScrapeConfigsShouldYieldToDifferentHashes
    ]
  ]

