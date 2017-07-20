import           Control.Lens             (review, (^.))
import           Data.Functor.Classes     (Eq1)
import           Data.Functor.Identity    (runIdentity)
import           Data.Monoid              ((<>))

import           Test.QuickCheck          (Arbitrary (..), suchThat)
import           Test.QuickCheck.Checkers (EqProp (..), Test, TestBatch, eq)
import           Test.QuickCheck.Classes  (applicative, functor)
import           Test.Tasty               (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit         (testCase, (@?=))
import           Test.Tasty.QuickCheck    (testProperty)

import           Control.Exitcode         (Exitcode, ExitcodeT, exitfailure0,
                                           exitsuccess, exitsuccess0,
                                           runExitcode, _ExitFailure, exitCode)

import           System.Exit             (ExitCode (..))

newtype EW f a = EW { unEW :: ExitcodeT f a } deriving (Eq, Show)

instance (Applicative f, Arbitrary a) => Arbitrary (EW f a) where
  arbitrary = fmap (EW . pure) arbitrary

instance Functor f => Functor (EW f) where
  fmap f = EW . fmap f . unEW

instance Applicative f => Applicative (EW f) where
  pure = EW . pure
  EW f <*> EW a = EW (f <*> a)

instance (Eq1 f, Eq a) => EqProp (EW f a) where
  (=-=) = eq

type CheckMe = EW [] (Integer, Integer, Integer)

newtype NonZero = NonZero Int deriving Show

instance Arbitrary NonZero where
  arbitrary = NonZero <$> suchThat arbitrary (/= 0)

main :: IO ()
main = defaultMain test_Exitcode

test_Exitcode :: TestTree
test_Exitcode =
  testGroup "Exitcode" [
    tastyCheckersBatch $ functor (undefined :: CheckMe)
  , tastyCheckersBatch $ applicative (undefined :: CheckMe)
  , applicativeTest
  , noLeft0Test
  ]

applicativeTest :: TestTree
applicativeTest =
  testGroup "Applicative" [
    testCase "Sticks to the Right" $
      pure (<> "bar") <*> pure "foo" @?= (exitsuccess "foobar" :: Exitcode String)
  ]

noLeft0Test :: TestTree
noLeft0Test =
  testGroup "Left 0 is impossible" [
    testProperty "_ExitFailure for non-zero input" $
      \(NonZero n) -> review _ExitFailure n == exitfailure0 n
  , testCase "_ExitFailure 0" $
      review _ExitFailure 0 @?= (exitsuccess0)

  , testProperty "exitfailure0 with non-zero input" $
      \(NonZero n) -> (runIdentity . runExitcode) (exitfailure0 n) == Left n
  , testCase "exitfailure0 0" $
      (runIdentity . runExitcode) (exitfailure0 0) @?= Right ()

  , testProperty "exitCode Iso with non-zero input" $
      \(NonZero n) -> runIdentity (exitfailure0 n ^. exitCode) == ExitFailure n
  , testCase "exitCode Iso 0" $
      runIdentity (exitfailure0 0 ^. exitCode) @?= ExitSuccess
  ]

tastyCheckersBatch :: TestBatch -> TestTree
tastyCheckersBatch (name, tests) =
  testGroup (name <> " laws") (tastyCheckersProperty <$> tests)

tastyCheckersProperty :: Test -> TestTree
tastyCheckersProperty =
  uncurry testProperty
