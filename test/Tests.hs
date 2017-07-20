import           Control.Lens             (review)
import           Data.Functor.Classes     (Eq1)
import           Data.Monoid              ((<>))

import           Test.QuickCheck          (Arbitrary (..))
import           Test.QuickCheck.Checkers (TestBatch, Test, EqProp (..), eq)
import           Test.QuickCheck.Classes  (applicative, functor)
import           Test.Tasty               (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit         (testCase, (@?=))
import           Test.Tasty.QuickCheck    (testProperty)

import           Control.Exitcode         (Exitcode, ExitcodeT, exitfailure0,
                                           exitsuccess, exitsuccess0,
                                           _ExitFailure)

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
    testProperty "_ExitFailure does not produce `Left 0`" $
      \n ->
        let ec = review _ExitFailure n
         in if n == 0 then ec == exitsuccess0 else ec == exitfailure0 n
  ]

tastyCheckersBatch :: TestBatch -> TestTree
tastyCheckersBatch (name, tests) =
  testGroup (name <> " laws") (tastyCheckersProperty <$> tests)

tastyCheckersProperty :: Test -> TestTree
tastyCheckersProperty =
  uncurry testProperty
