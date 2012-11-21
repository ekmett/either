{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             UndecidableInstances #-}
module Control.Monad.Trans.Either
  ( EitherT(..)
  , eitherT
  , mapEitherT
  , hoistEither
  , left
  , right
  ) where

import Control.Applicative
import Data.Functor.Bind
import Data.Functor.Plus
import Data.Foldable
import Data.Function (on)
import Data.Traversable
import Data.Semigroup
import Control.Monad.Trans.Class
-- import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad (liftM)
import Control.Monad.State (MonadState,get,put)
import Control.Monad.Random (MonadRandom,getRandom,getRandoms,
                             getRandomR,getRandomRs)

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }
-- TODO: Data, Typeable

instance Show (m (Either e a)) => Show (EitherT e m a) where
  showsPrec d (EitherT m) = showParen (d > 10) $
    showString "EitherT " . showsPrec 11 m

instance Read (m (Either e a)) => Read (EitherT e m a) where
  readsPrec d = readParen (d > 10)
    (\r' -> [ (EitherT m, t)
            | ("EitherT", s) <- lex r'
            , (m, t) <- readsPrec 11 s])

instance Eq (m (Either e a)) => Eq (EitherT e m a) where
  (==) = (==) `on` runEitherT

instance Ord (m (Either e a)) => Ord (EitherT e m a) where
  compare = compare `on` runEitherT


eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT m) = m >>= \z -> case z of
  Left a -> f a
  Right b -> g b
{-# INLINE eitherT #-}

left :: Monad m => e -> EitherT e m a
left = EitherT . return . Left
{-# INLINE left #-}

right :: Monad m => a -> EitherT e m a
right = return
{-# INLINE right #-}


mapEitherT :: Functor m => (e -> f) -> (a -> b) -> EitherT e m a -> EitherT f m b
mapEitherT f g (EitherT m) = EitherT (fmap h m) where
  h (Left e)  = Left (f e)
  h (Right a) = Right (g a)
{-# INLINE mapEitherT #-}

hoistEither :: Monad m => Either e a -> EitherT e m a
hoistEither = EitherT . return
{-# INLINE hoistEither #-}

instance Functor m => Functor (EitherT e m) where
  fmap f = EitherT . fmap (fmap f) . runEitherT

instance (Functor m, Monad m) => Apply (EitherT e m) where
  EitherT f <.> EitherT v = EitherT $ f >>= \mf -> case mf of
    Left  e -> return (Left e)
    Right k -> v >>= \mv -> case mv of
      Left  e -> return (Left e)
      Right x -> return (Right (k x))

instance (Functor m, Monad m) => Applicative (EitherT e m) where
  pure a  = EitherT $ return (Right a)
  EitherT f <*> EitherT v = EitherT $ f >>= \mf -> case mf of
    Left  e -> return (Left e)
    Right k -> v >>= \mv -> case mv of
      Left  e -> return (Left e)
      Right x -> return (Right (k x))

instance Monad m => Semigroup (EitherT e m a) where
  EitherT m <> EitherT n = EitherT $ m >>= \a -> case a of
    Left _ -> n
    Right r -> return (Right r)

instance (Functor m, Monad m) => Alt (EitherT e m) where
  (<!>) = (<>)

instance (Functor m, Monad m) => Bind (EitherT e m) where
  (>>-) = (>>=)

instance Monad m => Monad (EitherT e m) where
  return a = EitherT $ return (Right a)
  m >>= k  = EitherT $ do
    a <- runEitherT m
    case a of
      Left  l -> return (Left l)
      Right r -> runEitherT (k r)

{-
instance Monad m => MonadError e (EitherT e m) where
  throwError = EitherT . return . Left
  EitherT m `catchError` h = EitherT $ m >>= \a -> case a of
    Left  l -> runEitherT (h l)
    Right r -> return (Right r)
-}

instance MonadFix m => MonadFix (EitherT e m) where
  mfix f = EitherT $ mfix $ \a -> runEitherT $ f $ case a of
    Right r -> r
    _       -> error "empty mfix argument"

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance MonadIO m => MonadIO (EitherT e m) where
  liftIO = lift . liftIO

instance MonadState s m => MonadState s (EitherT e m) where
  get = lift get
  put = lift . put

instance MonadRandom m => MonadRandom (EitherT e m) where
  getRandom   = lift getRandom
  getRandoms  = lift getRandoms
  getRandomR  = lift . getRandomR
  getRandomRs = lift . getRandomRs

instance Foldable m => Foldable (EitherT e m) where
  foldMap f = foldMap (either mempty f) . runEitherT

instance (Traversable f) => Traversable (EitherT e f) where
  traverse f (EitherT a) =
    EitherT <$> traverse (either (pure . Left) (fmap Right . f)) a
