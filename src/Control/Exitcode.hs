{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE CPP                   #-}

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
                        , fromExitCode
                        -- * Extraction
                        , runExitcode
                        -- * Optics
                        , exitCode
                        , _ExitFailure
                        , _ExitSuccess
                        ) where

# if MIN_VERSION_base(4,9,0)
import           Control.Applicative        (Applicative, liftA2)
# else
import           Control.Applicative        (liftA2)
# endif
import           Control.Lens               (Iso, Prism', iso, prism', view,
                                             (^?), _Left, _Right)
import           Control.Monad.Cont.Class   (MonadCont (..))
import           Control.Monad.Error.Class  (MonadError (..))
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Reader       (MonadReader (ask, local))
import           Control.Monad.RWS.Class    (MonadRWS)
import           Control.Monad.State.Lazy   (MonadState (get, put))
import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           Control.Monad.Trans.Maybe  (MaybeT (MaybeT))
import           Control.Monad.Writer.Class (MonadWriter (listen, pass, tell, writer))
import           Data.Functor.Alt           (Alt, (<!>))
import           Data.Functor.Apply         (Apply, liftF2, (<.>))
import           Data.Functor.Bind          (Bind, (>>-))
# if MIN_VERSION_base(4,9,0)
import           Data.Functor.Classes       (Eq1, Ord1, Show1, compare1, eq1,
                                             liftCompare, liftEq, liftShowList,
                                             liftShowsPrec, showsPrec1,
                                             showsUnaryWith)
# else
import           Data.Functor.Classes       (Eq1, Ord1, Show1, compare1, eq1,
                                             showsPrec1, showsUnary1)
# endif
import           Data.Functor.Extend        (Extend, duplicated)
import           Data.Functor.Identity      (Identity (Identity))
import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup             (Semigroup, (<>))
import           Data.Semigroup.Foldable    (Foldable1)
import           System.Exit                (ExitCode (ExitFailure, ExitSuccess))

-- | An exit code status where failing with a value `0` cannot be represented.
--
-- Transformer for either a non-zero exit code (`Int`) or a value :: `a`.
data ExitcodeT f a =
  ExitcodeT (f (Either Int a))

type Exitcode a =
  ExitcodeT Identity a

type ExitcodeT0 f =
  ExitcodeT f ()

type Exitcode0 =
  Exitcode ()

-- | Construct a succeeding exit code with the given value.
exitsuccess ::
  Applicative f =>
  a
  -> ExitcodeT f a
exitsuccess =
  ExitcodeT . pure . Right

-- | Construct a succeeding exit code with unit.
exitsuccess0 ::
  Applicative f =>
  ExitcodeT0 f
exitsuccess0 =
  exitsuccess ()

-- | Construct a failing exit code with the given status.
--
-- If the given status is `0` then the exit code will succeed with unit.
exitfailure0 ::
  Applicative f =>
  Int
  -> ExitcodeT0 f
exitfailure0 n =
  if n == 0
    then
      exitsuccess0
    else
      ExitcodeT . pure . Left $ n

fromExitCode ::
  Functor f =>
  f ExitCode
  -> ExitcodeT0 f
fromExitCode x =
  let ExitcodeT (MaybeT r) = view exitCode x
  in  ExitcodeT (fromMaybe (Right ()) <$> r)

exitCode ::
  (Functor f, Functor g) =>
  Iso
    (f ExitCode)
    (g ExitCode)
    (ExitcodeT0 (MaybeT f))
    (ExitcodeT0 (MaybeT g))
exitCode =
  iso
    (\x -> ExitcodeT (MaybeT ((\e ->  case e of
                                        ExitSuccess ->
                                          Just (Right ())
                                        ExitFailure 0 ->
                                          Nothing
                                        ExitFailure n ->
                                          Just (Left n)) <$> x)))
    (\(ExitcodeT (MaybeT x)) -> (\e ->  case e of
                                          Just (Right ()) ->
                                            ExitSuccess
                                          Nothing ->
                                            ExitFailure 0
                                          Just (Left n) ->
                                            ExitFailure n) <$> x)

runExitcode ::
  ExitcodeT f a
  -> f (Either Int a)
runExitcode (ExitcodeT x) =
  x

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

instance (Bind f, Monad f) => Bind (ExitcodeT f) where
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

instance Eq1 f => Eq1 (ExitcodeT f) where
# if MIN_VERSION_base(4,9,0)
  liftEq f (ExitcodeT a) (ExitcodeT b) =
    liftEq (liftEq f) a b
# else
  eq1 (ExitcodeT a) (ExitcodeT b) =
   eq1 a b
# endif

instance (Ord1 f, Ord a) => Ord (ExitcodeT f a) where
  ExitcodeT a `compare` ExitcodeT b =
    a `compare1` b

instance (Ord1 f) => Ord1 (ExitcodeT f) where
# if MIN_VERSION_base(4,9,0)
  liftCompare f (ExitcodeT a) (ExitcodeT b) =
    liftCompare (liftCompare f) a b
# else
  compare1 (ExitcodeT a) (ExitcodeT b) =
    compare1 a b
# endif

instance (Show1 f, Show a) => Show (ExitcodeT f a) where
  showsPrec d (ExitcodeT m) =
# if MIN_VERSION_base(4,9,0)
    showsUnaryWith showsPrec1 "ExitcodeT" d m
# else
    showsUnary1 "ExitcodeT" d m
# endif

instance Show1 f => Show1 (ExitcodeT f) where
# if MIN_VERSION_base(4,9,0)
  liftShowsPrec sp sl d (ExitcodeT fa) =
    let showsPrecF = liftA2 liftShowsPrec (uncurry liftShowsPrec) (uncurry liftShowList) (sp, sl)
     in showsUnaryWith showsPrecF "ExitcodeT" d fa
# else
  showsPrec1 d (ExitcodeT fa) =
    showsUnary1 "ExitcodeT" d fa
# endif

instance Foldable f => Foldable (ExitcodeT f) where
  foldr f z (ExitcodeT x) =
    foldr (flip (foldr f)) z x

instance Foldable1 f => Foldable1 (ExitcodeT f)

instance Traversable f => Traversable (ExitcodeT f) where
  traverse f (ExitcodeT x) =
    ExitcodeT <$> traverse (traverse f) x

instance MonadIO f => MonadIO (ExitcodeT f) where
  liftIO io =
    ExitcodeT (Right <$> liftIO io)

instance MonadTrans ExitcodeT where
  lift = ExitcodeT . (>>= pure . pure)

instance MonadReader r f => MonadReader r (ExitcodeT f) where
  ask = lift ask
  local f (ExitcodeT m) = ExitcodeT $ local f m

instance MonadWriter w f => MonadWriter w (ExitcodeT f) where
  writer t = ExitcodeT . fmap pure $ writer t
  listen (ExitcodeT m) =
     ExitcodeT ((\(e, w) -> (,w) <$> e) <$> listen m)
  tell = ExitcodeT . fmap Right . tell
  pass e = do
    ((a, f), w) <- listen e
    tell (f w)
    pure a

instance MonadState s f => MonadState s (ExitcodeT f) where
  get = ExitcodeT (fmap Right get)
  put = ExitcodeT . fmap Right . put

instance MonadError e f => MonadError e (ExitcodeT f) where
  throwError = ExitcodeT . fmap Right . throwError
  catchError (ExitcodeT f) h =
     ExitcodeT $ flip catchError (runExitcode . h) f

instance MonadRWS r w s f => MonadRWS r w s (ExitcodeT f)

-- Given the embedded `Either` we can only handle computations that use `Either`.
-- This code taken from the ExceptT instance:
--   https://hackage.haskell.org/package/transformers-0.5.4.0/docs/src/Control.Monad.Trans.Except.html#line-237
instance MonadCont f => MonadCont (ExitcodeT f) where
  callCC = liftCallCC callCC

liftCallCC :: Functor f => (((Either Int a -> f (Either Int b)) -> f (Either Int a)) -> f (Either Int a))
           -> ((a -> ExitcodeT f b) -> ExitcodeT f a)
           -> ExitcodeT f a
liftCallCC callCC' f =
  ExitcodeT . callCC' $
    \c -> runExitcode (f (\a -> ExitcodeT (c (Right a))))
