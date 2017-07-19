import           Data.Functor.Classes     (Eq1)
import           Test.QuickCheck          (Arbitrary (..))
import           Test.QuickCheck.Checkers (EqProp (..), eq, quickBatch)
import           Test.QuickCheck.Classes  (applicative, functor)
import           Test.Tasty               (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit         (testCase, (@?=))

import           Control.Exitcode

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
main = do
  quickBatch $ functor (undefined :: CheckMe)
  quickBatch $ applicative (undefined :: CheckMe)
  defaultMain testApplicative

testApplicative :: TestTree
testApplicative =
  testGroup "Applicative" [
    testCase "Sticks to the Right" $
      pure (+1) <*> pure (41 :: Integer) @?= ExitcodeT [Right 42]
  ]
