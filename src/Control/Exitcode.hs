{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Control.Exitcode (
-- * Types
  ExitcodeT
, Exitcode
, ExitcodeT0
, Exitcode0
, ExitcodeT1
, Exitcode1
-- * Construction
, exitsuccess
, exitsuccess0
, exitfailure
, exitfailure0
, exitcodeValue
, exitcodeValue0
, fromExitCode
, fromExitCode'
, liftExitcode
, liftExitcodeError
, liftExitcodeError0
, hoistExitcode
, embedExitcode
, exitcode1
-- * Extraction
, runExitcodeT
, runExitcodeT0
, runExitcode
, runExitcode0
, runExitcodeT1
, runExitcode1
-- * Optics
, exitCode
, _ExitcodeInt
, _ExitcodeInt'
, _ExitFailure
, _ExitFailureError
, _ExitSuccess
, _Exitcode1
) where

import Control.Applicative
    ( Applicative((<*>), pure, liftA2) )
import Control.Category ( Category((.)) )
import Control.Lens
    ( preview,
      view,
      iso,
      _Left,
      prism,
      over,
      Field1(_1),
      Field2(_2),
      Iso,
      Lens,
      Prism,
      Traversal,
      Traversal' )
import Control.Monad ( join, Monad(return, (>>=)) )
import Control.Monad.Cont.Class ( MonadCont(..) )
import Control.Monad.Error.Class ( MonadError(..) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Reader ( MonadReader(ask, local) )
import Control.Monad.RWS.Class ( MonadRWS )
import Control.Monad.State.Lazy ( MonadState(get, put) )
import Control.Monad.Trans.Maybe ( MaybeT(MaybeT) )
import Control.Monad.Writer.Class ( MonadWriter(..) )
import Data.Bifoldable ( Bifoldable(bifoldMap) )
import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Bitraversable ( Bitraversable(..) )
import Data.Bool ( bool )
import Data.Either ( Either(..), either )
import Data.Eq ( Eq((==)) )
import Data.Foldable ( Foldable(foldMap) )
import Data.Function ( ($), const )
import Data.Functor ( Functor(fmap), (<$>) )
import Data.Functor.Alt ( Alt((<!>)) )
import Data.Functor.Apply ( Apply((<.>)) )
import Data.Functor.Bind ( Bind((>>-)) )
import Data.Functor.Classes
    ( compare1,
      eq1,
      showsPrec1,
      showsUnaryWith,
      Eq1(..),
      Ord1(..),
      Show1(..) )
import Data.Functor.Extend ( Extend(..) )
import Data.Functor.Identity ( Identity(Identity, runIdentity) )
import Data.Int ( Int )
import Data.Maybe ( Maybe(Nothing, Just), fromMaybe )
import Data.Monoid ( Monoid(mempty) )
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
-- Transformer for either a (non-zero exit code value (`Int`) with error :: `e`) or a (value :: `a`).
newtype ExitcodeT f e a =
  ExitcodeT (f (Either (e, Int) a))

type Exitcode e a =
  ExitcodeT Identity e a

type ExitcodeT0 f =
  ExitcodeT f () ()

type Exitcode0 =
  Exitcode () ()

-- | Construct a succeeding exit code with the given value.
--
-- >>> exitsuccess "abc" :: ExitcodeT Identity () String
-- ExitcodeT (Identity (Right "abc"))
exitsuccess ::
  Applicative f =>
  a
  -> ExitcodeT f e a
exitsuccess =
  ExitcodeT . pure . Right

-- | Construct a succeeding exit code with unit.
--
-- >>> exitsuccess0 :: ExitcodeT0 Identity
-- ExitcodeT (Identity (Right ()))
exitsuccess0 ::
  Applicative f =>
  ExitcodeT f e ()
exitsuccess0 =
  exitsuccess ()

-- | Construct a failing exit code with the given status.
--
-- If the given status is `0` then the exit code will succeed with unit.
--
-- >>> exitfailure 'x' 99 :: ExitcodeT Identity Char ()
-- ExitcodeT (Identity (Left ('x',99)))
exitfailure ::
  Applicative f =>
  e
  -> Int
  -> ExitcodeT f e ()
exitfailure e n =
  exitcodeValue e n ()

-- | Construct a failing exit code with the given status.
--
-- If the given status is `0` then the exit code will succeed with unit.
exitfailure0 ::
  Applicative f =>
  Int
  -> ExitcodeT0 f
exitfailure0 =
  exitfailure ()

-- | Construct an exit code with the given status.
-- Associate a value of type `e` with a failing exit code and a value of the type `a` with a success exit code.
--
-- If the given status is `0` then the exit code will succeed with unit.
-- >>> exitcodeValue 'x' 99 "abc" :: ExitcodeT Identity Char String
-- ExitcodeT (Identity (Left ('x',99)))
-- >>> exitcodeValue 'x' 0 "abc" :: ExitcodeT Identity Char String
-- ExitcodeT (Identity (Right "abc"))
exitcodeValue ::
  Applicative f =>
  e
  -> Int
  -> a
  -> ExitcodeT f e a
exitcodeValue e n =
  liftExitcodeError (pure (e, n))

-- | Construct an exit code with the given status.
--
-- If the given status is `0` then the exit code will succeed with unit.
--
-- >>> exitcodeValue0 99 :: ExitcodeT0 Identity
-- ExitcodeT (Identity (Left ((),99)))
-- >>> exitcodeValue0 0 :: ExitcodeT0 Identity
-- ExitcodeT (Identity (Right ()))
exitcodeValue0 ::
  Applicative f =>
  Int
  -> ExitcodeT0 f
exitcodeValue0 n =
  exitcodeValue () n ()

-- | From base exitcode.
--
-- >>> fromExitCode (Identity ExitSuccess)
-- ExitcodeT (Identity (Right ()))
-- >>> fromExitCode (Identity (ExitFailure 99))
-- ExitcodeT (Identity (Left ((),99)))
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
-- ExitcodeT (Identity (Left ((),99)))
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
-- ExitcodeT (MaybeT (Identity (Just (Left ((),99)))))
-- >>> view exitCode (Identity ExitSuccess)
-- ExitcodeT (MaybeT (Identity (Just (Right ()))))
-- >>> Control.Lens.review exitCode (exitfailure () 99) :: Identity ExitCode
-- Identity (ExitFailure 99)
-- >>> Control.Lens.review exitCode exitsuccess0 :: Identity ExitCode
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
                                  Just (Left ((), n))) <$> x)))
    (\(ExitcodeT (MaybeT x)) -> (\case
                                  Just (Right ()) ->
                                    ExitSuccess
                                  Nothing ->
                                    ExitFailure 0
                                  Just (Left ((), n)) ->
                                    ExitFailure n) <$> x)

-- | Extract either the non-zero value or the success value.
--
-- >>> runExitcodeT exitsuccess0 :: Identity (Either ((), Int) ())
-- Identity (Right ())
-- >>> runExitcodeT (exitfailure () 99) :: Identity (Either ((), Int) ())
-- Identity (Left ((),99))
runExitcodeT ::
  ExitcodeT f e a
  -> f (Either (e, Int) a)
runExitcodeT (ExitcodeT x) =
  x

-- | Extract either the non-zero value or `Nothing`.
--
-- >>> runExitcodeT0 exitsuccess0 :: Identity (Maybe Int)
-- Identity Nothing
-- >>> runExitcodeT0 (exitfailure () 99) :: Identity (Maybe Int)
-- Identity (Just 99)
runExitcodeT0 ::
  Functor f =>
  ExitcodeT0 f
  -> f (Maybe Int)
runExitcodeT0 x =
  preview (_Left . _2) <$> runExitcodeT x

-- | Extract either the non-zero value or the success value.
--
-- >>> runExitcode exitsuccess0 :: Either ((), Int) ()
-- Right ()
-- >>> runExitcode (exitfailure () 99) :: Either ((), Int) ()
-- Left ((),99)
runExitcode ::
  Exitcode e a
  -> Either (e, Int) a
runExitcode =
  runIdentity . runExitcodeT

-- | Extract either the non-zero value or `Nothing`.
--
-- >>> runExitcode0 exitsuccess0 :: Maybe Int
-- Nothing
-- >>> runExitcode0 (exitfailure () 99) :: Maybe Int
-- Just 99
runExitcode0 ::
  Exitcode0
  -> Maybe Int
runExitcode0 =
  runIdentity . runExitcodeT0

-- | Isomorphism to integer.
--
-- >>> view _ExitcodeInt exitsuccess0 :: [Int]
-- [0]
-- >>> view _ExitcodeInt (exitfailure0 99) :: [Int]
-- [99]
-- >>> review _ExitcodeInt [0]
-- ExitcodeT [Right ()]
-- >>> review _ExitcodeInt [99]
-- ExitcodeT [Left ((),99)]
_ExitcodeInt ::
  (Functor f, Functor f') =>
  Iso
    (ExitcodeT0 f)
    (ExitcodeT0 f')
    (f Int)
    (f' Int)
_ExitcodeInt =
  iso
    (\(ExitcodeT x) -> fmap (either (view _2) (\() -> 0)) x)
    (\x -> liftExitcodeError (((),) <$> x) ())

-- | Setter to integer.
--
-- >>> > preview _ExitcodeInt' (exitsuccess0 :: ExitcodeT [] () ())
-- Just 0
-- >>> preview _ExitcodeInt' (exitfailure0 99 :: ExitcodeT [] () ())
-- Just 99
-- >>> preview _ExitcodeInt' (exitfailure0 0 :: ExitcodeT [] () ())
-- Just 0
-- >>> over _ExitcodeInt' (subtract 1) exitsuccess0 :: ExitcodeT0 Identity
-- ExitcodeT (Identity (Left ((),-1)))
-- >>> over _ExitcodeInt' (subtract 1) (exitfailure0 99) :: ExitcodeT0 Identity
-- ExitcodeT (Identity (Left ((),98)))
-- >>> over _ExitcodeInt' (subtract 1) (exitfailure0 1) :: ExitcodeT0 Identity
-- ExitcodeT (Identity (Right ()))
_ExitcodeInt' ::
  Traversable f =>
  Traversal'
    (ExitcodeT0 f)
    Int
_ExitcodeInt' =
  _ExitcodeInt . traverse

-- | A traversal to exit failure.
--
-- >>> preview _ExitFailure (exitfailure () 99 :: ExitcodeT0 Identity)
-- Just ((),99)
-- >>> preview _ExitFailure (exitsuccess0 :: ExitcodeT0 Identity)
-- Nothing
-- >>> over _ExitFailure (\(e, n) -> (e + 1, n + 1)) (exitsuccess0 :: ExitcodeT Identity Int ())
-- ExitcodeT (Identity (Right ()))
-- >>> over _ExitFailure (\(e, n) -> (reverse e, n + 1)) (exitfailure "abc" 1 :: ExitcodeT Identity String ())
-- ExitcodeT (Identity (Left ("cba",2)))
-- >>> over _ExitFailure (\(e, n) -> (reverse e, n - 1)) (exitfailure "abc" 1 :: ExitcodeT Identity String ())
-- ExitcodeT (Identity (Right ()))
_ExitFailure ::
  Traversable f =>
  Traversal
    (ExitcodeT f e ())
    (ExitcodeT f e' ())
    (e, Int)
    (e', Int)
_ExitFailure f (ExitcodeT x) =
  ExitcodeT <$> traverse (either (\z -> runExitcodeT (liftExitcodeError (f z) ())) (pure . pure)) x

-- | A traversal over the associated failing value.
--
-- >>> over _ExitFailureError reverse exitsuccess0 :: ExitcodeT Identity [Int] ()
-- ExitcodeT (Identity (Right ()))
-- >>> over _ExitFailureError reverse (exitfailure "abc" 99) :: ExitcodeT Identity String ()
-- ExitcodeT (Identity (Left ("cba",99)))
-- >>> over _ExitFailureError reverse (exitfailure "abc" 0) :: ExitcodeT Identity String ()
-- ExitcodeT (Identity (Right ()))
-- >>> preview _ExitFailureError (exitfailure () 99 :: ExitcodeT0 Identity)
-- Just ()
-- >>> preview _ExitFailureError (exitsuccess0 :: ExitcodeT0 Identity)
-- Nothing
_ExitFailureError ::
  Traversable f =>
  Traversal
    (ExitcodeT f e a)
    (ExitcodeT f e' a)
    e
    e'
_ExitFailureError f (ExitcodeT x) =
  ExitcodeT <$> traverse (either (\(e, n) -> (\e' -> Left (e', n)) <$> f e) (pure . Right)) x

-- | A prism to exit success.
--
-- >>> over _ExitSuccess (\x -> x) (exitfailure0 99)
-- ExitcodeT (Identity (Left ((),99)))
-- >>> over _ExitSuccess (\x -> x) (exitfailure0 0)
-- ExitcodeT (Identity (Right ()))
-- >>> over _ExitSuccess (\x -> x) (exitfailure0 0)
-- ExitcodeT (Identity (Right ()))
-- >>> preview _ExitSuccess (exitfailure () 99)
-- Nothing
-- >>> preview _ExitSuccess exitsuccess0
-- Just ()
-- >>> Control.Lens.review _ExitSuccess "abc" :: ExitcodeT Identity () String
-- ExitcodeT (Identity (Right "abc"))
_ExitSuccess ::
  Prism
    (Exitcode e a)
    (Exitcode e a')
    a
    a'
_ExitSuccess =
  prism
    exitsuccess
    (\(ExitcodeT (Identity x)) ->
      over _Left (ExitcodeT . Identity . Left) x
    )

instance Functor f => Functor (ExitcodeT f e) where
  fmap f (ExitcodeT x) =
    ExitcodeT (fmap (fmap f) x)

instance Monad f => Apply (ExitcodeT f e) where
  ExitcodeT f <.> ExitcodeT a =
    ExitcodeT (f >>= either (pure . Left) (\f' -> fmap (fmap f') a))

instance Monad f => Applicative (ExitcodeT f e) where
  pure =
    ExitcodeT . pure . pure
  ExitcodeT f <*> ExitcodeT a =
    ExitcodeT (f >>= either (pure . Left) (\f' -> fmap (fmap f') a))

-- |
--
-- >>> exitsuccess "abc" >>- \s -> exitsuccess (reverse s) :: ExitcodeT Identity () String
-- ExitcodeT (Identity (Right "cba"))
-- >>> exitsuccess "abc" >>- \_ -> exitfailure () 99 :: ExitcodeT Identity () ()
-- ExitcodeT (Identity (Left ((),99)))
-- >>> exitfailure 'x' 99 >>- \_ -> exitsuccess "abc" :: ExitcodeT Identity Char String
-- ExitcodeT (Identity (Left ('x',99)))
-- >>> exitfailure 'x' 99 >>- \_ -> exitfailure 'y' 88 :: ExitcodeT Identity Char ()
-- ExitcodeT (Identity (Left ('x',99)))
-- >>> let loop = loop in exitfailure () 99 >>- loop :: ExitcodeT Identity () ()
-- ExitcodeT (Identity (Left ((),99)))
instance Monad f => Bind (ExitcodeT f e) where
  (>>-) =
    (>>=)

instance Monad f => Monad (ExitcodeT f e) where
  return =
    ExitcodeT . return . return
  ExitcodeT x >>= f =
    ExitcodeT
      (x >>= either (pure . Left) (runExitcodeT . f))

-- |
--
-- >>> exitsuccess "abc" <!> exitsuccess "def" :: ExitcodeT Identity () String
-- ExitcodeT (Identity (Right "abc"))
-- >>> exitsuccess "abc" <!> exitcodeValue () 99 "def" :: ExitcodeT Identity () String
-- ExitcodeT (Identity (Right "abc"))
-- >>> exitcodeValue 'x' 99 "abc" <!> exitsuccess "def" :: ExitcodeT Identity Char String
-- ExitcodeT (Identity (Right "def"))
-- >>> exitcodeValue 'x' 99 "abc" <!> exitcodeValue 'y' 88 "def" :: ExitcodeT Identity Char String
-- ExitcodeT (Identity (Left ('y',88)))
instance Monad f => Alt (ExitcodeT f e) where
  ExitcodeT a <!> ExitcodeT b =
    ExitcodeT (a >>= either (const b) (pure a))

-- |
--
-- >>> exitsuccess "abc" <> exitsuccess "def" :: ExitcodeT Identity () String
-- ExitcodeT (Identity (Right "abcdef"))
-- >>> exitsuccess "abc" <> exitcodeValue () 99 "def" :: ExitcodeT Identity () String
-- ExitcodeT (Identity (Right "abc"))
-- >>> exitcodeValue 'x' 99 "abc" <> exitsuccess "def" :: ExitcodeT Identity Char String
-- ExitcodeT (Identity (Right "def"))
-- >>> exitcodeValue 'x' 99 "abc" <> exitcodeValue 'y' 88 "def" :: ExitcodeT Identity Char String
-- ExitcodeT (Identity (Left ('y',88)))
instance (Semigroup a, Applicative f) => Semigroup (ExitcodeT f e a) where
  ExitcodeT a <> ExitcodeT b =
    let jn (Left _) x  = x
        jn x (Left _) = x
        jn (Right a1) (Right a2) = Right (a1 <> a2)
    in  ExitcodeT (liftA2 jn a b)

-- |
--
-- >>> mempty :: ExitcodeT Identity () String
-- ExitcodeT (Identity (Right ""))
instance (Monoid a, Applicative f) => Monoid (ExitcodeT f e a) where
  mempty =
    ExitcodeT (pure (Right mempty))

-- |
--
-- >>> duplicated (exitfailure () 0) :: ExitcodeT Identity () (ExitcodeT Identity () ())
-- ExitcodeT (Identity (Right (ExitcodeT (Identity (Right ())))))
-- >>> duplicated (exitfailure () 99) :: ExitcodeT Identity () (ExitcodeT Identity () ())
-- ExitcodeT (Identity (Right (ExitcodeT (Identity (Left ((),99))))))
-- >>> duplicated (exitsuccess "abc") :: ExitcodeT Identity () (ExitcodeT Identity () String)
-- ExitcodeT (Identity (Right (ExitcodeT (Identity (Right "abc")))))
instance Extend f => Extend (ExitcodeT f e) where
  duplicated (ExitcodeT x) =
    ExitcodeT (extended (Right . ExitcodeT) x)

instance (Eq1 f, Eq e, Eq a) => Eq (ExitcodeT f e a) where
  ExitcodeT a == ExitcodeT b =
    a `eq1` b

instance (Eq1 f, Eq e) => Eq1 (ExitcodeT f e) where
  liftEq f (ExitcodeT a) (ExitcodeT b) =
    liftEq (liftEq f) a b

instance (Ord1 f, Ord e, Ord a) => Ord (ExitcodeT f e a) where
  ExitcodeT a `compare` ExitcodeT b =
    a `compare1` b

instance (Ord1 f, Ord e) => Ord1 (ExitcodeT f e) where
  liftCompare f (ExitcodeT a) (ExitcodeT b) =
    liftCompare (liftCompare f) a b

instance (Show1 f, Show e, Show a) => Show (ExitcodeT f e a) where
  showsPrec d (ExitcodeT m) =
    showsUnaryWith showsPrec1 "ExitcodeT" d m

instance (Show1 f, Show e) => Show1 (ExitcodeT f e) where
  liftShowsPrec sp sl d (ExitcodeT fa) =
    let showsPrecF = liftA2 liftShowsPrec (uncurry liftShowsPrec) (uncurry liftShowList) (sp, sl)
    in showsUnaryWith showsPrecF "ExitcodeT" d fa

instance Foldable f => Foldable (ExitcodeT f e) where
  foldMap f (ExitcodeT x) =
    foldMap (foldMap f) x

-- |
--
-- >>> traverse (\x -> x) [exitfailure 'x' 99] :: ExitcodeT Identity Char [()]
-- ExitcodeT (Identity (Left ('x',99)))
-- >>> traverse (\x -> x) [exitfailure 'x' 99, exitsuccess0] :: ExitcodeT Identity Char [()]
-- ExitcodeT (Identity (Left ('x',99)))
-- >>> traverse (\x -> x) [exitfailure 'x' 99, exitsuccess0, exitfailure 'y' 88] :: ExitcodeT Identity Char [()]
-- ExitcodeT (Identity (Left ('x',99)))
-- >>> traverse (\x -> x) [exitsuccess0, exitfailure 'x' 88] :: ExitcodeT Identity Char [()]
-- ExitcodeT (Identity (Left ('x',88)))
-- >>> traverse (\x -> x) [exitsuccess0] :: ExitcodeT Identity () [()]
-- ExitcodeT (Identity (Right [()]))
instance Traversable f => Traversable (ExitcodeT f e) where
  traverse f (ExitcodeT x) =
    ExitcodeT <$> traverse (traverse f) x

instance MonadIO f => MonadIO (ExitcodeT f e) where
  liftIO io =
    ExitcodeT (Right <$> liftIO io)

liftExitcode ::
  Functor f =>
  f a
  -> ExitcodeT f e a
liftExitcode x =
  ExitcodeT (Right <$> x)

liftExitcodeError ::
  Functor f =>
  f (e, Int)
  -> a
  -> ExitcodeT f e a
liftExitcodeError x a =
  ExitcodeT ((\(e, n) -> bool (Left (e, n)) (Right a) (n == 0)) <$> x)

liftExitcodeError0 ::
  Functor f =>
  f Int
  -> ExitcodeT f () ()
liftExitcodeError0 x =
  liftExitcodeError (((),) <$> x) ()

hoistExitcode ::
  (forall x. f x -> g x)
  -> ExitcodeT f e a
  -> ExitcodeT g e a
hoistExitcode nat (ExitcodeT x) =
  ExitcodeT (nat x)

embedExitcode ::
  Functor g =>
  (forall x. f x -> ExitcodeT g e x)
  -> ExitcodeT f e a
  -> ExitcodeT g e a
embedExitcode nat (ExitcodeT x) =
  ExitcodeT (join <$> runExitcodeT (nat x))

instance MonadReader r f => MonadReader r (ExitcodeT f e) where
  ask =
    liftExitcode ask
  local f (ExitcodeT m) =
    ExitcodeT (local f m)

-- |
--
-- >>> writer ('x', "abc") :: ExitcodeT ((,) String) () Char
-- ExitcodeT ("abc",Right 'x')
-- >>> listen (exitfailure 'x' 99 :: ExitcodeT ((,) String) Char ())
-- ExitcodeT ("",Left ('x',99))
-- >>> listen (exitsuccess 99 :: ExitcodeT ((,) String) () Int)
-- ExitcodeT ("",Right (99,""))
-- >>> tell "abc" :: ExitcodeT ((,) String) () ()
-- ExitcodeT ("abc",Right ())
-- >>> pass (exitsuccess ('x', reverse)) :: ExitcodeT ((,) String) () Char
-- ExitcodeT ("",Right 'x')
-- >>> pass (('x', reverse) <$ (exitfailure 'x' 99 :: ExitcodeT ((,) String) Char ()))
-- ExitcodeT ("",Left ('x',99))
instance MonadWriter w f => MonadWriter w (ExitcodeT f e) where
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

instance MonadState s f => MonadState s (ExitcodeT f e) where
  get =
    ExitcodeT (fmap Right get)
  put =
    ExitcodeT . fmap Right . put

-- |
--
-- >>> throwError 99 :: ExitcodeT (Either Int) () String
-- ExitcodeT (Left 99)
-- >>> catchError exitsuccess0 (exitfailure 'x') :: ExitcodeT (Either Int) Char ()
-- ExitcodeT (Right (Right ()))
-- >>> catchError (exitfailure 'x' 99) (\_ -> exitsuccess0) :: ExitcodeT (Either Int) Char ()
-- ExitcodeT (Right (Left ('x',99)))
-- >>> catchError (exitfailure 'x' 99) (exitfailure 'y') :: ExitcodeT (Either Int) Char ()
-- ExitcodeT (Right (Left ('x',99)))
-- >>> catchError exitsuccess0 (\_ -> exitsuccess0) :: ExitcodeT (Either Int) () ()
-- ExitcodeT (Right (Right ()))
instance MonadError e f => MonadError e (ExitcodeT f e') where
  throwError =
    ExitcodeT . fmap Right . throwError
  catchError (ExitcodeT f) h =
     ExitcodeT (catchError f (runExitcodeT . h))

instance MonadRWS r w s f => MonadRWS r w s (ExitcodeT f e)

-- Given the embedded `Either` we can only handle computations that use `Either`.
-- This code taken from the ExceptT instance:
--   https://hackage.haskell.org/package/transformers-0.5.4.0/docs/src/Control.Monad.Trans.Except.html#line-237
instance MonadCont f => MonadCont (ExitcodeT f e) where
  callCC =
    let liftCallCC callCC' f =
          ExitcodeT . callCC' $
            \c -> runExitcodeT (f (ExitcodeT . c . Right))
    in  liftCallCC callCC

instance Functor f => Bifunctor (ExitcodeT f) where
  bimap f g (ExitcodeT x) =
    ExitcodeT (fmap (bimap (over _1 f) g) x)

instance Foldable f => Bifoldable (ExitcodeT f) where
  bifoldMap f g (ExitcodeT x) =
    foldMap (bifoldMap (f . view _1) g) x

instance Traversable f => Bitraversable (ExitcodeT f) where
  bitraverse f g (ExitcodeT x) =
    ExitcodeT <$> traverse (bitraverse (\(a, n) -> (, n) <$> f a) g) x

type ExitcodeT1 f a =
  ExitcodeT f a a

type Exitcode1 a =
  ExitcodeT1 Identity a

-- | Construct an exitcode with an associated value.
--
-- >>> exitcode1 99 "abc" :: ExitcodeT1 Identity String
-- ExitcodeT (Identity (Left ("abc",99)))
-- >>> exitcode1 0 "abc" :: ExitcodeT1 Identity String
-- ExitcodeT (Identity (Right "abc"))
exitcode1 ::
  Applicative f =>
  Int
  -> a
  -> ExitcodeT1 f a
exitcode1 n a =
  exitcodeValue a n a

-- | Extract either the non-zero value or the success value.
--
-- >>> runExitcodeT1 exitsuccess0
-- Right ()
-- >>> runExitcodeT1 (exitfailure () 99) :: Identity (Either ((), Int) ())
-- Identity (Left ((),99))
-- >>> runExitcodeT1 (exitcode1 0 "abc") :: Identity (Either (String, Int) String)
-- Identity (Right "abc")
-- >>> runExitcodeT1 (exitcode1 99 "abc") :: Identity (Either (String, Int) String)
-- Identity (Left ("abc",99))
runExitcodeT1 ::
  ExitcodeT1 f a
  -> f (Either (a, Int) a)
runExitcodeT1 (ExitcodeT x) =
  x

-- | Extract either the non-zero value or the success value.
--
-- >>> runExitcode1 exitsuccess0
-- Right ()
-- >>> runExitcode1 (exitfailure () 99)
-- Left ((),99)
-- >>> runExitcode1 (exitcode1 0 "abc")
-- Right "abc"
-- >>> runExitcode1 (exitcode1 99 "abc")
-- Left ("abc",99)
runExitcode1 ::
  Exitcode1 a
  -> Either (a, Int) a
runExitcode1 =
  runIdentity . runExitcodeT1

-- | A lens to the value associated with an exitcode.
--
-- >>> view _Exitcode1 (exitcode1 0 "abc")
-- "abc"
-- >>> view _Exitcode1 (exitcode1 99 "abc")
-- "abc"
-- >>> view _Exitcode1 (exitcodeValue "abc" 0 "def")
-- "def"
-- >>> view _Exitcode1 (exitcodeValue "abc" 99 "def")
-- "abc"
-- >>> over _Exitcode1 reverse (exitcode1 0 "abc")
-- ExitcodeT (Identity (Right "cba"))
-- >>> over _Exitcode1 reverse (exitcode1 99 "abc")
-- ExitcodeT (Identity (Left ("cba",99)))
-- >>> over _Exitcode1 reverse (exitcodeValue "abc" 0 "def")
-- ExitcodeT (Identity (Right "fed"))
-- >>> over _Exitcode1 reverse (exitcodeValue "abc" 99 "def")
-- ExitcodeT (Identity (Left ("cba",99)))
_Exitcode1 ::
  Lens
    (Exitcode1 a)
    (Exitcode1 a')
    a
    a'
_Exitcode1 f =
  either
    (\(a, n) -> fmap (exitcode1 n) (f a))
    (fmap (exitcode1 0) . f)
    . runExitcode1
