{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE CPP                   #-}

module Control.Exitcode (
                        -- * Types
                          ExitcodeT
                        , Exitcode
                        , Exitcode'
                        , ExitcodeT0
                        , ExitcodeT0'
                        , Exitcode0
                        , Exitcode0'
                        -- * Construction
                        , exitsuccess
                        , exitsuccess0
                        , exitfailure0
                        , fromExitCode
                        , fromExitCode'
                        -- * Extraction
                        , runExitcodeT
                        , runExitcode
                        -- * Optics
                        , exitCode
                        , liftIso
                        , _ExitFailure
                        , _ExitFailure'
                        , _ExitFailureE
                        , _ExitSuccess
                        ) where

import           Control.Applicative        (Applicative(pure, (<*>)), liftA2)
import           Control.Category           ((.))
import           Control.Lens               (Iso, Prism, Traversal', Traversal, iso, prism, view, over,
                                             _Left, _1, _2)
import           Control.Monad              (Monad(return, (>>=)))
import           Control.Monad.Cont.Class   (MonadCont (..))
import           Control.Monad.Error.Class  (MonadError (..))
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Morph        (MFunctor(hoist), MMonad(embed))
import           Control.Monad.Reader       (MonadReader (ask, local))
import           Control.Monad.RWS.Class    (MonadRWS)
import           Control.Monad.State.Lazy   (MonadState (get, put))
import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           Control.Monad.Trans.Maybe  (MaybeT (MaybeT))
import           Control.Monad.Writer.Class (MonadWriter (listen, pass, tell, writer))
import           Data.Either                (Either(Left, Right), either)
import           Data.Eq                    (Eq((==)))
import           Data.Foldable              (Foldable(foldr))
import           Data.Function              (($), const, flip)
import           Data.Functor               (Functor(fmap), (<$>))
import           Data.Functor.Alt           (Alt, (<!>))
import           Data.Functor.Apply         (Apply, liftF2, (<.>))
import           Data.Functor.Bind          (Bind, (>>-))
# if MIN_VERSION_transformers(0,5,0)
import           Data.Functor.Classes       (Eq1, Ord1, Show1, compare1, eq1,
                                             liftCompare, liftEq, liftShowList,
                                             liftShowsPrec, showsPrec1,
                                             showsUnaryWith)
# else
import           Data.Functor.Classes       (Eq1, Ord1, Show1, compare1, eq1,
                                             showsPrec1, showsUnary1)
# endif
import           Data.Functor.Extend        (Extend, duplicated)
import           Data.Functor.Identity      (Identity (Identity, runIdentity))
import           Data.Int                   (Int)
import           Data.Maybe                 (Maybe(Just, Nothing), fromMaybe)
import           Data.Ord                   (Ord(compare))
import           Data.Semigroup             (Semigroup, (<>))
import           Data.Semigroup.Foldable    (Foldable1)
import           Data.Traversable           (Traversable(traverse))
import           Data.Tuple                 (uncurry)
import           Prelude                    (Show(showsPrec))
import           System.Exit                (ExitCode (ExitFailure, ExitSuccess))

-- | An exit code status where failing with a value `0` cannot be represented.
--
-- Transformer for either a non-zero exit code (`Int`) or a value :: `a`.
newtype ExitcodeT e f a =
  ExitcodeT (f (Either (e, Int) a))

type Exitcode e a =
  ExitcodeT e Identity a

type Exitcode' a =
  Exitcode () a

type ExitcodeT0 f e =
  ExitcodeT e f ()

type ExitcodeT0' f =
  ExitcodeT0 f ()

type Exitcode0 e =
  Exitcode e ()

type Exitcode0' =
  Exitcode' ()

-- | Construct a succeeding exit code with the given value.
exitsuccess ::
  Applicative f =>
  a
  -> ExitcodeT e f a
exitsuccess =
  ExitcodeT . pure . Right

-- | Construct a succeeding exit code with unit.
exitsuccess0 ::
  Applicative f =>
  ExitcodeT0 f e
exitsuccess0 =
  exitsuccess ()

-- | Construct a failing exit code with the given status.
--
-- If the given status is `0` then the exit code will succeed with unit.
exitfailure0 ::
  Applicative f =>
  e
  -> Int
  -> ExitcodeT0 f e
exitfailure0 e n =
  if n == 0
    then
      exitsuccess0
    else
      ExitcodeT . pure . Left $ (e, n)

exitCode ::
  (Functor f, Functor g) =>
  Iso
    (f ExitCode)
    (g ExitCode)
    (ExitcodeT0' (MaybeT f))
    (ExitcodeT0' (MaybeT g))
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

fromExitCode ::
  Functor f =>
  f ExitCode
  -> ExitcodeT0' f
fromExitCode x =
  let ExitcodeT (MaybeT r) = view exitCode x
  in  ExitcodeT (fromMaybe (Right ()) <$> r)

fromExitCode' ::
  ExitCode
  -> Exitcode0'
fromExitCode' =
  fromExitCode . Identity

runExitcodeT ::
  ExitcodeT e f a
  -> f (Either (e, Int) a)
runExitcodeT (ExitcodeT x) =
  x

runExitcode ::
  Exitcode e a
  -> Either (e, Int) a
runExitcode =
  runIdentity . runExitcodeT

_ExitFailure ::
  Prism
    (Exitcode0 e)
    (Exitcode0 e')
    (e, Int)
    (e', Int)
_ExitFailure =
  prism
    (uncurry exitfailure0)
    (\(ExitcodeT (Identity x)) ->
      case x of
        Left (e, n) -> Right (e, n)
        Right () -> Left (ExitcodeT (Identity (Right ()))))

_ExitFailure' ::
  Traversal'
    Exitcode0'
    Int
_ExitFailure' =
  _ExitFailure . _2

_ExitFailureE ::
  Traversal
    (Exitcode0 e)
    (Exitcode0 e')
    e
    e'
_ExitFailureE =
  _ExitFailure . _1

_ExitSuccess ::
  Prism
    (Exitcode e a)
    (Exitcode e b)
    a
    b
_ExitSuccess =
  prism
    exitsuccess
    (\(ExitcodeT (Identity x)) ->
      over _Left (ExitcodeT . Identity . Left) x
    )

liftIso ::
  (Functor f, Functor g) =>
  Iso
    (ExitcodeT e f a)
    (ExitcodeT e' g b)
    (f (Exitcode e a))
    (g (Exitcode e' b))
liftIso =
  iso
    (\(ExitcodeT x) -> fmap (ExitcodeT . Identity) x)
    (ExitcodeT . fmap runExitcode)

instance Functor f => Functor (ExitcodeT e f) where
  fmap f (ExitcodeT x) =
    ExitcodeT (fmap (fmap f) x)

instance Apply f => Apply (ExitcodeT e f) where
  ExitcodeT f <.> ExitcodeT a =
    ExitcodeT (liftF2 (<.>) f a)

instance Applicative f => Applicative (ExitcodeT e f) where
  pure =
    ExitcodeT . pure . pure
  ExitcodeT f <*> ExitcodeT a =
    ExitcodeT (liftA2 (<*>) f a)

instance (Bind f, Monad f) => Bind (ExitcodeT e f) where
  (>>-) =
    (>>=)

instance Monad f => Monad (ExitcodeT e f) where
  return =
    ExitcodeT . return . return
  ExitcodeT x >>= f =
    ExitcodeT
      (x >>= either (pure . Left) (\a -> let ExitcodeT y = f a in y))

instance Monad f => Alt (ExitcodeT e f) where
  ExitcodeT a <!> ExitcodeT b =
    ExitcodeT (a >>= either (const b) (pure a))

instance Monad f => Semigroup (ExitcodeT e f a) where
  ExitcodeT a <> ExitcodeT b =
    ExitcodeT (a >>= either (const b) (pure a))

instance Applicative f => Extend (ExitcodeT e f) where
  duplicated (ExitcodeT x) =
    ExitcodeT ((pure <$>) <$> x )

instance (Eq1 f, Eq e, Eq a) => Eq (ExitcodeT e f a) where
  ExitcodeT a == ExitcodeT b =
    a `eq1` b

instance (Eq e, Eq1 f) => Eq1 (ExitcodeT e f) where
# if MIN_VERSION_transformers(0,5,0)
  liftEq f (ExitcodeT a) (ExitcodeT b) =
    liftEq (liftEq f) a b
# else
  eq1 (ExitcodeT a) (ExitcodeT b) =
   eq1 a b
# endif

instance (Ord1 f, Ord e, Ord a) => Ord (ExitcodeT e f a) where
  ExitcodeT a `compare` ExitcodeT b =
    a `compare1` b

instance (Ord1 f, Ord e) => Ord1 (ExitcodeT e f) where
# if MIN_VERSION_transformers(0,5,0)
  liftCompare f (ExitcodeT a) (ExitcodeT b) =
    liftCompare (liftCompare f) a b
# else
  compare1 (ExitcodeT a) (ExitcodeT b) =
    compare1 a b
# endif

instance (Show1 f, Show e, Show a) => Show (ExitcodeT e f a) where
  showsPrec d (ExitcodeT m) =
# if MIN_VERSION_transformers(0,5,0)
    showsUnaryWith showsPrec1 "ExitcodeT" d m
# else
    showsUnary1 "ExitcodeT" d m
# endif

instance (Show e, Show1 f) => Show1 (ExitcodeT e f) where
# if MIN_VERSION_transformers(0,5,0)
  liftShowsPrec sp sl d (ExitcodeT fa) =
    let showsPrecF = liftA2 liftShowsPrec (uncurry liftShowsPrec) (uncurry liftShowList) (sp, sl)
     in showsUnaryWith showsPrecF "ExitcodeT" d fa
# else
  showsPrec1 d (ExitcodeT fa) =
    showsUnary1 "ExitcodeT" d fa
# endif

instance Foldable f => Foldable (ExitcodeT e f) where
  foldr f z (ExitcodeT x) =
    foldr (flip (foldr f)) z x

instance Foldable1 f => Foldable1 (ExitcodeT e f)

instance Traversable f => Traversable (ExitcodeT e f) where
  traverse f (ExitcodeT x) =
    ExitcodeT <$> traverse (traverse f) x

instance MonadIO f => MonadIO (ExitcodeT e f) where
  liftIO io =
    ExitcodeT (Right <$> liftIO io)

instance MonadTrans (ExitcodeT e) where
  lift = ExitcodeT . (>>= pure . pure)

instance MonadReader r f => MonadReader r (ExitcodeT e f) where
  ask = lift ask
  local f (ExitcodeT m) = ExitcodeT $ local f m

instance MonadWriter w f => MonadWriter w (ExitcodeT e f) where
  writer t = ExitcodeT . fmap pure $ writer t
  listen (ExitcodeT m) =
     ExitcodeT ((\(e, w) -> (,w) <$> e) <$> listen m)
  tell = ExitcodeT . fmap Right . tell
  pass e = do
    ((a, f), w) <- listen e
    tell (f w)
    pure a

instance MonadState s f => MonadState s (ExitcodeT e f) where
  get = ExitcodeT (fmap Right get)
  put = ExitcodeT . fmap Right . put

instance MonadError e f => MonadError e (ExitcodeT e f) where
  throwError = ExitcodeT . fmap Right . throwError
  catchError (ExitcodeT f) h =
     ExitcodeT $ flip catchError (runExitcodeT . h) f

instance MonadRWS r w s f => MonadRWS r w s (ExitcodeT e f)

-- Given the embedded `Either` we can only handle computations that use `Either`.
-- This code taken from the ExceptT instance:
--   https://hackage.haskell.org/package/transformers-0.5.4.0/docs/src/Control.Monad.Trans.Except.html#line-237
instance MonadCont f => MonadCont (ExitcodeT e f) where
  callCC = liftCallCC callCC

liftCallCC :: Functor f => (((Either (e, Int) a -> f (Either (e, Int) b)) -> f (Either (e, Int) a)) -> f (Either (e, Int) a))
           -> ((a -> ExitcodeT e f b) -> ExitcodeT e f a)
           -> ExitcodeT e f a
liftCallCC callCC' f =
  ExitcodeT . callCC' $
    \c -> runExitcodeT (f (ExitcodeT . c . Right))

instance MFunctor (ExitcodeT e) where
  hoist nat (ExitcodeT x) =
    ExitcodeT (nat x)

instance MMonad (ExitcodeT e) where
  embed nat (ExitcodeT x) =
    let ex (Left e) = Left e
        ex (Right (Left e)) = Left e
        ex (Right (Right a)) = Right a
    in  ExitcodeT (fmap ex (let ExitcodeT y = nat x in y))
