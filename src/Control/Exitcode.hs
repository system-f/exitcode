{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Control.Exitcode (
-- * Types
  ExitcodeT
, Exitcode
, ExitcodeT0
, Exitcode0
-- * Construction
, exitsuccess
, exitsuccess0
, exitfailure0
, exitCodeValue
, fromExitCode
, fromExitCode'
-- * Extraction
, runExitcode
-- * Optics
, exitCode
, _ExitFailure
, _ExitSuccess
) where

import Control.Applicative
    ( Applicative((<*>), liftA2, pure) )
import Control.Category ( Category((.)) )
import Control.Lens
    ( (^?),
      view,
      iso,
      _Left,
      prism,
      prism',
      over,
      Iso,
      Prism,
      Prism' )
import Control.Monad ( join, Monad(return, (>>=)) )
import Control.Monad.Cont.Class ( MonadCont(..) )
import Control.Monad.Error.Class ( MonadError(..) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Morph
    ( MFunctor(..), MMonad(..) )
import Control.Monad.Reader ( MonadReader(ask, local) )
import Control.Monad.RWS.Class
    ( MonadRWS )
import Control.Monad.State.Lazy
    ( MonadState(get, put) )
import Control.Monad.Trans.Class ( MonadTrans(..) )
import Control.Monad.Trans.Maybe ( MaybeT(MaybeT) )
import Control.Monad.Writer.Class ( MonadWriter(..) )
import Data.Bool
import Data.Either ( Either(..), either )
import Data.Eq ( Eq((==)) )
import Data.Foldable ( Foldable(foldr) )
import Data.Function ( ($), const, flip )
import Data.Functor ( Functor(fmap), (<$>) )
import Data.Functor.Alt ( Alt((<!>)) )
import Data.Functor.Apply ( Apply((<.>)) )
import Data.Functor.Bind ( Bind((>>-)) )
import Data.Functor.Classes (Eq1, Ord1, Show1, compare1, eq1,
                                             liftCompare, liftEq, liftShowList,
                                             liftShowsPrec, showsPrec1,
                                             showsUnaryWith)
import Data.Functor.Extend ( Extend(..) )
import Data.Functor.Identity ( Identity(Identity) )
import Data.Int ( Int )
import Data.Maybe ( Maybe(Nothing, Just), fromMaybe )
import Data.Monoid hiding (Alt)
import Data.Ord ( Ord(compare) )
import Data.Semigroup ( Semigroup((<>)) )
import Data.Traversable ( Traversable(traverse) )
import Data.Tuple ( uncurry )
import GHC.Show ( Show(showsPrec) )
import System.Exit ( ExitCode(..) )

-- $setup
-- >>> import Prelude
-- >>> import Control.Lens

-- | An exit code status where failing with a value `0` cannot be represented.
--
-- Transformer for either a non-zero exit code (`Int`) or a value :: `a`.
newtype ExitcodeT f a =
  ExitcodeT (f (Either Int a))

type Exitcode a =
  ExitcodeT Identity a

type ExitcodeT0 f =
  ExitcodeT f ()

type Exitcode0 =
  Exitcode ()

-- | Construct a succeeding exit code with the given value.
--
-- >>> exitsuccess "abc" :: ExitcodeT Identity String
-- ExitcodeT (Identity (Right "abc"))
exitsuccess ::
  Applicative f =>
  a
  -> ExitcodeT f a
exitsuccess =
  ExitcodeT . pure . Right

-- | Construct a succeeding exit code with unit.
--
-- >>> exitsuccess0 :: ExitcodeT0 Identity
-- ExitcodeT (Identity (Right ()))
exitsuccess0 ::
  Applicative f =>
  ExitcodeT0 f
exitsuccess0 =
  exitsuccess ()

-- | Construct a failing exit code with the given status.
--
-- If the given status is `0` then the exit code will succeed with unit.
--
-- >>> exitfailure0 99 :: ExitcodeT0 Identity
-- ExitcodeT (Identity (Left 99))
-- >>> exitsuccess "abc" <> exitCodeValue 99 "def" :: ExitcodeT Identity String
-- ExitcodeT (Identity (Right "abc"))
-- >>> exitCodeValue 99 "abc" <> exitsuccess "def" :: ExitcodeT Identity String
-- ExitcodeT (Identity (Right "def"))
-- >>> exitCodeValue 99 "abc" <> exitCodeValue 88 "def" :: ExitcodeT Identity String
-- ExitcodeT (Identity (Left 88))
exitfailure0 ::
  Applicative f =>
  Int
  -> ExitcodeT0 f
exitfailure0 n =
  exitCodeValue n ()

-- |
--
-- >>> exitCodeValue 99 "abc" :: ExitcodeT Identity String
-- ExitcodeT (Identity (Left 99))
-- >>> exitCodeValue 0 "abc" :: ExitcodeT Identity String
-- ExitcodeT (Identity (Right "abc"))
exitCodeValue ::
  Applicative f =>
  Int
  -> a
  -> ExitcodeT f a
exitCodeValue n a =
  ExitcodeT (pure (bool (Left n) (Right a) (n == 0)))

-- | From base exitcode.
--
-- >>> fromExitCode (Identity ExitSuccess)
-- ExitcodeT (Identity (Right ()))
-- >>> fromExitCode (Identity (ExitFailure 99))
-- ExitcodeT (Identity (Left 99))
fromExitCode ::
  Functor f =>
  f ExitCode
  -> ExitcodeT0 f
fromExitCode x =
  let ExitcodeT (MaybeT r) = view exitCode x
  in  ExitcodeT (fromMaybe (Right ()) <$> r)

-- | From base exitcode.
--
-- >>> fromExitCode' ExitSuccess
-- ExitcodeT (Identity (Right ()))
-- >>> fromExitCode' (ExitFailure 99)
-- ExitcodeT (Identity (Left 99))
-- >>> fromExitCode' (ExitFailure 0)
-- ExitcodeT (Identity (Right ()))
fromExitCode' ::
  ExitCode
  -> Exitcode0
fromExitCode' =
  fromExitCode . Identity

-- | Isomorphism from base exitcode to underlying `Maybe (Either Int ())` where `Int` is non-zero.
--
-- >>> view exitCode (Identity (ExitFailure 99))
-- ExitcodeT (MaybeT (Identity (Just (Left 99))))
-- >>> view exitCode (Identity ExitSuccess)
-- ExitcodeT (MaybeT (Identity (Just (Right ()))))
-- >>> review exitCode (exitfailure0 99) :: Identity ExitCode
-- Identity (ExitFailure 99)
-- >>> review exitCode exitsuccess0 :: Identity ExitCode
-- Identity ExitSuccess
exitCode ::
  (Functor f, Functor g) =>
  Iso
    (f ExitCode)
    (g ExitCode)
    (ExitcodeT0 (MaybeT f))
    (ExitcodeT0 (MaybeT g))
exitCode =
  iso
    (\x -> ExitcodeT (MaybeT ((\case
                                ExitSuccess ->
                                  Just (Right ())
                                ExitFailure 0 ->
                                  Nothing
                                ExitFailure n ->
                                  Just (Left n)) <$> x)))
    (\(ExitcodeT (MaybeT x)) -> (\case
                                  Just (Right ()) ->
                                    ExitSuccess
                                  Nothing ->
                                    ExitFailure 0
                                  Just (Left n) ->
                                    ExitFailure n) <$> x)

-- | Extract either the non-zero value or the success value.
--
-- >>> runExitcode exitsuccess0 :: Identity (Either Int ())
-- Identity (Right ())
-- >>> runExitcode (exitfailure0 99) :: Identity (Either Int ())
-- Identity (Left 99)
runExitcode ::
  ExitcodeT f a
  -> f (Either Int a)
runExitcode (ExitcodeT x) =
  x

-- | A prism to exit failure.
--
-- >>> preview _ExitFailure (exitfailure0 99)
-- Just 99
-- >>> preview _ExitFailure exitsuccess0
-- Nothing
-- >>> review _ExitFailure 99
-- ExitcodeT (Identity (Left 99))
-- >>> review _ExitFailure 0
-- ExitcodeT (Identity (Right ()))
_ExitFailure ::
  Prism'
    Exitcode0
    Int
_ExitFailure =
  prism'
    exitfailure0
    (\(ExitcodeT (Identity x)) -> x ^? _Left)

-- | A prism to exit success.
--
-- >>> preview _ExitSuccess (exitfailure0 99)
-- Nothing
-- >>> preview _ExitSuccess exitsuccess0
-- Just ()
-- >>> review _ExitSuccess "abc"
-- ExitcodeT (Identity (Right "abc"))
_ExitSuccess ::
  Prism
    (Exitcode a)
    (Exitcode b)
    a
    b
_ExitSuccess =
  prism
    exitsuccess
    (\(ExitcodeT (Identity x)) ->
      over _Left (ExitcodeT . Identity . Left) x
    )

instance Functor f => Functor (ExitcodeT f) where
  fmap f (ExitcodeT x) =
    ExitcodeT (fmap (fmap f) x)

instance Monad f => Apply (ExitcodeT f) where
  ExitcodeT f <.> ExitcodeT a =
    ExitcodeT (f >>= either (pure . Left) (\f' -> fmap (fmap f') a))

instance Monad f => Applicative (ExitcodeT f) where
  pure =
    ExitcodeT . pure . pure
  ExitcodeT f <*> ExitcodeT a =
    ExitcodeT (f >>= either (pure . Left) (\f' -> fmap (fmap f') a))

-- |
--
-- >>> exitsuccess "abc" >>- \s -> exitsuccess (reverse s) :: ExitcodeT Identity String
-- ExitcodeT (Identity (Right "cba"))
-- >>> exitsuccess "abc" >>- \_ -> exitfailure0 99 :: ExitcodeT Identity ()
-- ExitcodeT (Identity (Left 99))
-- >>> exitfailure0 99 >>- \_ -> exitsuccess "abc" :: ExitcodeT Identity String
-- ExitcodeT (Identity (Left 99))
-- >>> exitfailure0 99 >>- \_ -> exitfailure0 88 :: ExitcodeT Identity ()
-- ExitcodeT (Identity (Left 99))
-- >>> let loop = loop in exitfailure0 99 >>- loop :: ExitcodeT Identity ()
-- ExitcodeT (Identity (Left 99))
instance Monad f => Bind (ExitcodeT f) where
  (>>-) =
    (>>=)

instance Monad f => Monad (ExitcodeT f) where
  return =
    ExitcodeT . return . return
  ExitcodeT x >>= f =
    ExitcodeT
      (x >>= either (pure . Left) (runExitcode . f))

instance Monad f => Alt (ExitcodeT f) where
  ExitcodeT a <!> ExitcodeT b =
    ExitcodeT (a >>= either (const b) (pure a))

-- |
--
-- >>> exitsuccess "abc" <> exitsuccess "def" :: ExitcodeT Identity String
-- ExitcodeT (Identity (Right "abcdef"))
instance (Semigroup a, Applicative f) => Semigroup (ExitcodeT f a) where
  ExitcodeT a <> ExitcodeT b =
    let jn (Left _) x  = x
        jn x (Left _) = x
        jn (Right a1) (Right a2) = Right (a1 <> a2)
    in  ExitcodeT (liftA2 jn a b)

instance (Monoid a, Applicative f) => Monoid (ExitcodeT f a) where
  mempty =
    ExitcodeT (pure (Right mempty))

-- |
--
-- >>> duplicated (exitfailure0 99) :: ExitcodeT Identity (ExitcodeT Identity ())
-- ExitcodeT (Identity (Right (ExitcodeT (Identity (Left 99)))))
-- >>> duplicated (exitsuccess "abc") :: ExitcodeT Identity (ExitcodeT Identity String)
-- ExitcodeT (Identity (Right (ExitcodeT (Identity (Right "abc")))))
instance Extend f => Extend (ExitcodeT f) where
  duplicated (ExitcodeT x) =
    ExitcodeT (extended (Right . ExitcodeT) x)

instance (Eq1 f, Eq a) => Eq (ExitcodeT f a) where
  ExitcodeT a == ExitcodeT b =
    a `eq1` b

instance Eq1 f => Eq1 (ExitcodeT f) where
  liftEq f (ExitcodeT a) (ExitcodeT b) =
    liftEq (liftEq f) a b

instance (Ord1 f, Ord a) => Ord (ExitcodeT f a) where
  ExitcodeT a `compare` ExitcodeT b =
    a `compare1` b

instance (Ord1 f) => Ord1 (ExitcodeT f) where
  liftCompare f (ExitcodeT a) (ExitcodeT b) =
    liftCompare (liftCompare f) a b

instance (Show1 f, Show a) => Show (ExitcodeT f a) where
  showsPrec d (ExitcodeT m) =
    showsUnaryWith showsPrec1 "ExitcodeT" d m

instance Show1 f => Show1 (ExitcodeT f) where
  liftShowsPrec sp sl d (ExitcodeT fa) =
    let showsPrecF = liftA2 liftShowsPrec (uncurry liftShowsPrec) (uncurry liftShowList) (sp, sl)
    in showsUnaryWith showsPrecF "ExitcodeT" d fa

instance Foldable f => Foldable (ExitcodeT f) where
  foldr f z (ExitcodeT x) =
    foldr (flip (foldr f)) z x

-- |
--
-- >>> traverse id [exitfailure0 99] :: ExitcodeT Identity [()]
-- ExitcodeT (Identity (Left 99))
-- >>> traverse id [exitfailure0 99, exitsuccess0] :: ExitcodeT Identity [()]
-- ExitcodeT (Identity (Left 99))
-- >>> traverse id [exitfailure0 99, exitsuccess0, exitfailure0 88] :: ExitcodeT Identity [()]
-- ExitcodeT (Identity (Left 99))
-- >>> traverse id [exitsuccess0, exitfailure0 88] :: ExitcodeT Identity [()]
-- ExitcodeT (Identity (Left 88))
-- >>> traverse id [exitsuccess0] :: ExitcodeT Identity [()]
-- ExitcodeT (Identity (Right [()]))
instance Traversable f => Traversable (ExitcodeT f) where
  traverse f (ExitcodeT x) =
    ExitcodeT <$> traverse (traverse f) x

instance MonadIO f => MonadIO (ExitcodeT f) where
  liftIO io =
    ExitcodeT (Right <$> liftIO io)

instance MonadTrans ExitcodeT where
  lift =
    ExitcodeT . (>>= pure . pure)

instance MonadReader r f => MonadReader r (ExitcodeT f) where
  ask =
    lift ask
  local f (ExitcodeT m) =
    ExitcodeT (local f m)

-- |
--
-- >>> writer ('x', "abc") :: ExitcodeT ((,) String) Char
-- ExitcodeT ("abc",Right 'x')
-- >>> listen (exitfailure0 99 :: ExitcodeT ((,) String) ())
-- ExitcodeT ("",Left 99)
-- >>> listen (exitsuccess 99 :: ExitcodeT ((,) String) Int)
-- ExitcodeT ("",Right (99,""))
-- >>> tell "abc" :: ExitcodeT ((,) String) ()
-- ExitcodeT ("abc",Right ())
-- >>> pass (exitsuccess ('x', reverse)) :: ExitcodeT ((,) String) Char
-- ExitcodeT ("",Right 'x')
-- >>> pass (('x', reverse) <$ (exitfailure0 99 :: ExitcodeT ((,) String) ()))
-- ExitcodeT ("",Left 99)
instance MonadWriter w f => MonadWriter w (ExitcodeT f) where
  writer t =
    ExitcodeT . fmap pure $ writer t
  listen (ExitcodeT m) =
    ExitcodeT ((\(e, w) -> (,w) <$> e) <$> listen m)
  tell =
    ExitcodeT . fmap Right . tell
  pass e =
    do  ((a, f), w) <- listen e
        tell (f w)
        pure a

instance MonadState s f => MonadState s (ExitcodeT f) where
  get =
    ExitcodeT (fmap Right get)
  put =
    ExitcodeT . fmap Right . put

-- |
--
-- >>> throwError 99 :: ExitcodeT (Either Int) String
-- ExitcodeT (Left 99)
-- >>> catchError exitsuccess0 exitfailure0 :: ExitcodeT (Either Int) ()
-- ExitcodeT (Right (Right ()))
-- >>> catchError (exitfailure0 99) (\_ -> exitsuccess0) :: ExitcodeT (Either Int) ()
-- ExitcodeT (Right (Left 99))
-- >>> catchError (exitfailure0 99) exitfailure0 :: ExitcodeT (Either Int) ()
-- ExitcodeT (Right (Left 99))
-- >>> catchError exitsuccess0 (\_ -> exitsuccess0) :: ExitcodeT (Either Int) ()
-- ExitcodeT (Right (Right ()))
instance MonadError e f => MonadError e (ExitcodeT f) where
  throwError =
    ExitcodeT . fmap Right . throwError
  catchError (ExitcodeT f) h =
     ExitcodeT (catchError f (runExitcode . h))

instance MonadRWS r w s f => MonadRWS r w s (ExitcodeT f)

-- Given the embedded `Either` we can only handle computations that use `Either`.
-- This code taken from the ExceptT instance:
--   https://hackage.haskell.org/package/transformers-0.5.4.0/docs/src/Control.Monad.Trans.Except.html#line-237
instance MonadCont f => MonadCont (ExitcodeT f) where
  callCC =
    let liftCallCC callCC' f =
          ExitcodeT . callCC' $
            \c -> runExitcode (f (ExitcodeT . c . Right))
    in  liftCallCC callCC

-- |
--
-- >>> hoist (\(Identity x) -> Just x) exitsuccess0
-- ExitcodeT (Just (Right ()))
-- >>> hoist (\(Identity x) -> Just x) (exitfailure0 99)
-- ExitcodeT (Just (Left 99))
instance MFunctor ExitcodeT where
  hoist nat (ExitcodeT x) =
    ExitcodeT (nat x)

instance MMonad ExitcodeT where
  embed nat (ExitcodeT x) =
    ExitcodeT (join <$> runExitcode (nat x))
