module Control.Exitcode where

import Control.Applicative
import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Alt
import Data.Functor.Bind
import Data.Functor.Classes
import Data.Functor.Extend
import Data.Semigroup
import Data.Semigroup.Foldable
import System.Exit

-- hide the constructor, `Left 0` is an invalid state
data ExitcodeT f a =
  ExitcodeT (f (Either Int a))

type Exitcode a =
  ExitcodeT Identity a

type Exitcode0 =
  Exitcode ()

exitsuccess ::
  a
  -> Exitcode a
exitsuccess =
  ExitcodeT . Identity . Right

exitsuccess0 ::
  Exitcode0
exitsuccess0 =
  exitsuccess ()

exitfailure0 ::
  Int
  -> Exitcode0
exitfailure0 n =
  if n == 0
    then
      exitsuccess0
    else
      ExitcodeT . Identity . Left $ n

exitcode ::
  Iso'
    Exitcode0
    ExitCode
exitcode =
  iso
    (\(ExitcodeT (Identity x)) -> either ExitFailure (const ExitSuccess) x)
    (\x ->  case x of
              ExitSuccess ->
                exitsuccess0
              ExitFailure 0 ->
                exitsuccess0
              ExitFailure n ->
                exitfailure0 n)

_ExitFailure ::
  Prism'
    Exitcode0
    Int
_ExitFailure =
  prism'
    exitfailure0
    (\(ExitcodeT (Identity x)) -> x ^? _Left)

_ExitSuccess ::
  Prism'
    Exitcode0
    ()
_ExitSuccess =
  prism'
    (\() -> exitsuccess0)
    (\(ExitcodeT (Identity x)) -> x ^? _Right)

instance Functor f => Functor (ExitcodeT f) where
  fmap f (ExitcodeT x) =
    ExitcodeT (fmap (fmap f) x)

instance Apply f => Apply (ExitcodeT f) where
  ExitcodeT f <.> ExitcodeT a =
    ExitcodeT (liftF2 (<.>) f a)

instance Applicative f => Applicative (ExitcodeT f) where
  pure =
    ExitcodeT . pure . pure
  ExitcodeT f <*> ExitcodeT a =
    ExitcodeT (liftA2 (<*>) f a)

instance (Apply f, Bind f, Monad f) => Bind (ExitcodeT f) where
  (>>-) =
    (>>=)

instance Monad f => Monad (ExitcodeT f) where
  return =
    ExitcodeT . return . return
  ExitcodeT x >>= f =
    ExitcodeT
      (x >>= either (pure . Left) (\a -> let ExitcodeT y = f a in y))

instance Monad f => Alt (ExitcodeT f) where
  ExitcodeT a <!> ExitcodeT b =
    ExitcodeT (a >>= either (const b) (pure a))

instance Monad f => Semigroup (ExitcodeT f a) where
  ExitcodeT a <> ExitcodeT b =
    ExitcodeT (a >>= either (const b) (pure a))

instance Applicative f => Extend (ExitcodeT f) where
  duplicated (ExitcodeT x) =
    ExitcodeT ((pure <$>) <$> x )

instance (Eq1 f, Eq a) => Eq (ExitcodeT f a) where
  ExitcodeT a == ExitcodeT b =
    a `eq1` b

instance (Ord1 f, Ord a) => Ord (ExitcodeT f a) where
  ExitcodeT a `compare` ExitcodeT b =
    a `compare1` b

instance (Show1 f, Show a) => Show (ExitcodeT f a) where
  showsPrec d (ExitcodeT m) =
    showsUnaryWith showsPrec1 "ExitcodeT" d m

instance Foldable f => Foldable (ExitcodeT f) where
  foldr f b (ExitcodeT m) =
    let g a b' = liftA2 either const (flip f) b' a
     in foldr g b m

instance Foldable1 f => Foldable1 (ExitcodeT f)

-- MonadReader, MonadWriter, MonadState, MonadRWS, MonadError
-- MonadFix, MonadFail, MonadCont
-- Foldable, Traversable, Foldable1, Traversable1
-- MonadTrans, MonadIO, MonadZip
-- Eq1, Ord1, Show1
