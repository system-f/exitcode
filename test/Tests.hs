import           Control.Lens              (review, (^.), (^?))
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import           Data.Functor.Classes      (Eq1)
import           Data.Functor.Identity     (Identity (..), runIdentity)
import           Data.Monoid               ((<>))

import           Hedgehog                  (forAll, property, (===))
import qualified Hedgehog.Gen              as Gen
import           Hedgehog.Internal.Gen     (MonadGen)
import qualified Hedgehog.Range            as Range
import           Test.QuickCheck           (Arbitrary)
import           Test.QuickCheck.Checkers  (EqProp (..), Test, TestBatch, eq)
import           Test.QuickCheck.Classes   (applicative, functor)
import           Test.Tasty                (TestTree, defaultMain, testGroup)
import           Test.Tasty.Hedgehog       (testProperty)
import           Test.Tasty.HUnit          (testCase, (@?=))
import qualified Test.Tasty.QuickCheck     as TQC

import           Control.Exitcode          (Exitcode, ExitcodeT, exitCode,
                                            exitfailure0, exitsuccess,
                                            exitsuccess0, runExitcode,
                                            runExitcodeT, _ExitFailure,
                                            _ExitSuccess, ExitcodeT0)

import           System.Exit               (ExitCode (..))

newtype EW f e a = EW { unEW :: ExitcodeT f e a } deriving (Eq, Show)

instance (Monad f, Arbitrary e, Arbitrary a) => Arbitrary (EW f e a) where
  arbitrary = fmap (EW . pure) TQC.arbitrary

instance Functor f => Functor (EW f e) where
  fmap f = EW . fmap f . unEW

instance Monad f => Applicative (EW f e) where
  pure = EW . pure
  EW f <*> EW a = EW (f <*> a)

instance (Eq1 f, Eq e, Eq a) => EqProp (EW f e a) where
  (=-=) = eq

type CheckMe = EW [] String (Integer, Integer, Integer)

nonZero :: MonadGen m => m Int
nonZero =
  let allOfTheInts = Range.linear (minBound :: Int) (maxBound :: Int)
   in Gen.filter (/= 0) (Gen.int allOfTheInts)

main :: IO ()
main = defaultMain test_Exitcode

test_Exitcode :: TestTree
test_Exitcode =
  testGroup "Exitcode" [
    tastyCheckersBatch $ functor (undefined :: CheckMe)
  , tastyCheckersBatch $ applicative (undefined :: CheckMe)
  , applicativeTest
  , exitFailureTraversalTest
  , exitSuccessPrismTest
  , exitfailure0Test
  , exitCodePrismTest
  ]

applicativeTest :: TestTree
applicativeTest =
  testGroup "Applicative" [
    testCase "Sticks to the Right" $
      pure (<> "bar") <*> pure "foo" @?= (exitsuccess "foobar" :: Exitcode String String)
  ]

exitFailureTraversalTest :: TestTree
exitFailureTraversalTest =
  testGroup "_ExitFailure Traversal" [
    testProperty "view non-zero input" . property $
      forAll nonZero >>= (\n -> (exitfailure0 n :: ExitcodeT0 Identity) ^? _ExitFailure === Just ((), n))
  , testCase "view 0" $
      (exitfailure0 0 :: ExitcodeT0 Identity) ^? _ExitFailure @?= Nothing
  ]

exitSuccessPrismTest :: TestTree
exitSuccessPrismTest =
  testGroup "_ExitSuccess Prism" [
    testCase "review" $
      review _ExitSuccess () @?= (exitsuccess0 :: ExitcodeT Identity () ())
  , testCase "view exitsuccess0" $
      (exitsuccess0 :: ExitcodeT Identity () ()) ^? _ExitSuccess @?= Just ()
  , testProperty "view exitfailure0 non-zero" . property $
      forAll nonZero >>= (\n -> exitfailure0 n ^? _ExitSuccess === Nothing)
  , testCase "view exitfailure0 0" $
      exitfailure0 0 ^? _ExitSuccess @?= Just ()
  ]

exitfailure0Test :: TestTree
exitfailure0Test =
  testGroup "exitfailure0" [
    testProperty "non-zero input" . property $
      forAll nonZero >>= (\n ->
        runExitcode (exitfailure0 n) === Left ((), n))
  , testCase "0" $
      runExitcode (exitfailure0 0) @?= Right ()
  ]

exitCodePrismTest :: TestTree
exitCodePrismTest =
  testGroup "exitCode Prism" [
    testProperty "`exitfailure0 n`, where n is non-zero" . property $
      forAll nonZero >>= \n -> (review exitCode (exitfailure0 n)) === Identity (ExitFailure n)
  , testCase "review `exitfailure 0`" $
      runIdentity (review exitCode (exitfailure0 0)) @?= ExitSuccess
  , testCase "review `exitsuccess0`" $
      runIdentity (review exitCode exitsuccess0) @?= ExitSuccess
  , testProperty "view ExitFailure n, where n is non-zero" . property $
      forAll nonZero >>= (\n -> Identity (ExitFailure n) ^? exitCode === Just (exitfailure0 n))
  , testCase "view ExitFailure 0" $
      let _ = ""
      in  runExitcodeT (Identity (ExitFailure 0) ^. exitCode) @?= (MaybeT (Identity Nothing))
  , testCase "view ExitSuccess" $
      Identity ExitSuccess ^? exitCode @?= Just exitsuccess0
  ]

tastyCheckersBatch :: TestBatch -> TestTree
tastyCheckersBatch (name, tests) =
  testGroup (name <> " laws") (tastyCheckersProperty <$> tests)

tastyCheckersProperty :: Test -> TestTree
tastyCheckersProperty =
  uncurry TQC.testProperty
