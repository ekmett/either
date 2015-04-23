{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Either
-- Copyright   :  (C) 2008-2014 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  MPTCs
--
-- This module provides a minimalist 'Either' monad transformer.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Either
  ( EitherT(..)
  , eitherT
  , bimapEitherT
  , mapEitherT
  , hoistEither
  , bracketEitherT
  , bracketEitherT_
  , left
  , right
  , swapEitherT
  ) where

import Control.Applicative
import Control.Monad (liftM, MonadPlus(..))
import Control.Monad.Base (MonadBase(..), liftBaseDefault)
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Free.Class
import Control.Monad.Catch as MonadCatch
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State (MonadState,get,put)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control (MonadBaseControl(..), MonadTransControl(..), defaultLiftBaseWith, defaultRestoreM)
import Control.Monad.Writer.Class
import Control.Monad.Random (MonadRandom,getRandom,getRandoms,getRandomR,getRandomRs)
import Data.Either.Combinators ( swapEither )
import Data.Foldable
import Data.Function (on)
import Data.Functor.Bind
import Data.Functor.Plus
import Data.Traversable
import Data.Semigroup

-- | 'EitherT' is a version of 'Control.Monad.Trans.Error.ErrorT' that does not
-- require a spurious 'Control.Monad.Error.Class.Error' instance for the 'Left'
-- case.
--
-- 'Either' is a perfectly usable 'Monad' without such a constraint. 'ErrorT' is
-- not the generalization of the current 'Either' monad, it is something else.
--
-- This is necessary for both theoretical and practical reasons. For instance an
-- apomorphism is the generalized anamorphism for this Monad, but it cannot be
-- written with 'ErrorT'.
--
-- In addition to the combinators here, the @errors@ package provides a large
-- number of combinators for working with this type.
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Show (m (Either e a)) => Show (EitherT e m a) where
  showsPrec d (EitherT m) = showParen (d > 10) $
    showString "EitherT " . showsPrec 11 m
  {-# INLINE showsPrec #-}

instance Read (m (Either e a)) => Read (EitherT e m a) where
  readsPrec d = readParen (d > 10)
    (\r' -> [ (EitherT m, t)
            | ("EitherT", s) <- lex r'
            , (m, t) <- readsPrec 11 s])
  {-# INLINE readsPrec #-}

instance Eq (m (Either e a)) => Eq (EitherT e m a) where
  (==) = (==) `on` runEitherT
  {-# INLINE (==) #-}

instance Ord (m (Either e a)) => Ord (EitherT e m a) where
  compare = compare `on` runEitherT
  {-# INLINE compare #-}

-- | Given a pair of actions, one to perform in case of failure, and one to perform
-- in case of success, run an 'EitherT' and get back a monadic result.
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT m) = m >>= \z -> case z of
  Left a -> f a
  Right b -> g b
{-# INLINE eitherT #-}

-- | Analogous to 'Left'. Equivalent to 'throwError'.
left :: Monad m => e -> EitherT e m a
left = EitherT . return . Left
{-# INLINE left #-}

-- | Analogous to 'Right'. Equivalent to 'return'.
right :: Monad m => a -> EitherT e m a
right = return
{-# INLINE right #-}

-- | Map over both failure and success.
bimapEitherT :: Functor m => (e -> f) -> (a -> b) -> EitherT e m a -> EitherT f m b
bimapEitherT f g (EitherT m) = EitherT (fmap h m) where
  h (Left e)  = Left (f e)
  h (Right a) = Right (g a)
{-# INLINE bimapEitherT #-}

-- | Map the unwrapped computation using the given function.
--
-- @
-- 'runEitherT' ('mapEitherT' f m) = f ('runEitherT' m)
-- @
mapEitherT :: (m (Either e a) -> n (Either e' b)) -> EitherT e m a -> EitherT e' n b
mapEitherT f m = EitherT $ f (runEitherT m)
{-# INLINE mapEitherT #-}

-- | Lift an 'Either' into an 'EitherT'
hoistEither :: Monad m => Either e a -> EitherT e m a
hoistEither = EitherT . return
{-# INLINE hoistEither #-}

-- | Acquire a resource in 'EitherT' and then perform an action with it,
-- cleaning up afterwards regardless of error. Like
-- 'Control.Exception.bracket', but acting only in 'EitherT'.
bracketEitherT :: Monad m => EitherT e m a -> (a -> EitherT e m b) -> (a -> EitherT e m c) -> EitherT e m c
bracketEitherT before after thing = do
    a <- before
    r <- thing a `catchError` (\err -> after a >> left err)
    -- If catchError already triggered, then `after` already ran *and* we are
    -- in a Left state, so `after` will not run again here.
    _ <- after a
    return r

-- | Version of 'bracketEitherT' which discards the result from the initial
-- action.
bracketEitherT_ :: Monad m => EitherT e m a -> EitherT e m b -> EitherT e m c -> EitherT e m c
bracketEitherT_ before after thing = do
    _ <- before
    r <- thing `catchError` (\err -> after >> left err)
    -- If catchError already triggered, then `after` already ran *and* we are
    -- in a Left state, so `after` will not run again here.
    _ <- after
    return r

-- | Monad transformer version of 'swapEither'.
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT = EitherT . fmap swapEither . runEitherT
{-# INLINE swapEitherT #-}

instance Functor m => Functor (EitherT e m) where
  fmap f = EitherT . fmap (fmap f) . runEitherT
  {-# INLINE fmap #-}

instance Apply m => Apply (EitherT e m) where
  (<.>) a b = EitherT $ (<.>) <$> runEitherT a <.> runEitherT b
  {-# INLINE (<.>) #-}

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . Right
  {-# INLINE pure #-}
  (<*>) a b = EitherT $ (<*>) <$> runEitherT a <*> runEitherT b
  {-# INLINE (<*>) #-}

instance (Applicative m, Monoid e) => Alternative (EitherT e m) where
  (<|>) a b = EitherT $ combine <$> runEitherT a <*> runEitherT b
    where 
      combine l r = case l of
        Left l -> case r of
          Left l' -> Left (mappend l l')
          Right r -> Right r
        Right r -> Right r
  {-# INLINE (<|>) #-}

  empty = EitherT $ pure (Left mempty)
  {-# INLINE empty #-}

instance (Applicative m, Monad m, Monoid e) => MonadPlus (EitherT e m) where
  mplus = (<|>)
  {-# INLINE mplus #-}

  mzero = empty
  {-# INLINE mzero #-}

instance Monad m => Semigroup (EitherT e m a) where
  EitherT m <> EitherT n = EitherT $ m >>= \a -> case a of
    Left _  -> n
    Right r -> return (Right r)
  {-# INLINE (<>) #-}

instance (Apply m, Semigroup e) => Alt (EitherT e m) where
  (<!>) a b = EitherT $ combine <$> runEitherT a <.> runEitherT b
    where 
      combine l r = case l of
        Left l -> case r of
          Left l' -> Left ((<>) l l')
          Right r -> Right r
        Right r -> Right r
  {-# INLINE (<!>) #-}

instance (Monad m, Apply m) => Bind (EitherT e m) where
  (>>-) = (>>=)
  {-# INLINE (>>-) #-}

instance Monad m => Monad (EitherT e m) where
  return a = EitherT $ return (Right a)
  {-# INLINE return #-}
  m >>= k  = EitherT $ do
    a <- runEitherT m
    case a of
      Left  l -> return (Left l)
      Right r -> runEitherT (k r)
  {-# INLINE (>>=) #-}
  fail = EitherT . fail
  {-# INLINE fail #-}

instance Monad m => MonadError e (EitherT e m) where
  throwError = EitherT . return . Left
  {-# INLINE throwError #-}
  EitherT m `catchError` h = EitherT $ m >>= \a -> case a of
    Left  l -> runEitherT (h l)
    Right r -> return (Right r)
  {-# INLINE catchError #-}

-- | Throws exceptions into the base monad.
instance MonadThrow m => MonadThrow (EitherT e m) where
  throwM = lift . throwM
  {-# INLINE throwM #-}

-- | Catches exceptions from the base monad.
instance MonadCatch m => MonadCatch (EitherT e m) where
  catch (EitherT m) f = EitherT $ MonadCatch.catch m (runEitherT . f)
  {-# INLINE catch #-}

instance MonadFix m => MonadFix (EitherT e m) where
  mfix f = EitherT $ mfix $ \a -> runEitherT $ f $ case a of
    Right r -> r
    _       -> error "empty mfix argument"
  {-# INLINE mfix #-}

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (EitherT e m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadCont m => MonadCont (EitherT e m) where
  callCC f = EitherT $
    callCC $ \c ->
    runEitherT (f (\a -> EitherT $ c (Right a)))
  {-# INLINE callCC #-}

instance MonadReader r m => MonadReader r (EitherT e m) where
  ask = lift ask
  {-# INLINE ask #-}
  local f (EitherT m) = EitherT (local f m)
  {-# INLINE local #-}

instance MonadState s m => MonadState s (EitherT e m) where
  get = lift get
  {-# INLINE get #-}
  put = lift . put
  {-# INLINE put #-}

instance MonadWriter s m => MonadWriter s (EitherT e m) where
  tell = lift . tell
  {-# INLINE tell #-}
  listen = mapEitherT $ \ m -> do
    (a, w) <- listen m
    return $! fmap (\ r -> (r, w)) a
  {-# INLINE listen #-}
  pass = mapEitherT $ \ m -> pass $ do
    a <- m
    return $! case a of
        Left  l      -> (Left  l, id)
        Right (r, f) -> (Right r, f)
  {-# INLINE pass #-}

instance MonadRandom m => MonadRandom (EitherT e m) where
  getRandom   = lift getRandom
  {-# INLINE getRandom #-}
  getRandoms  = lift getRandoms
  {-# INLINE getRandoms #-}
  getRandomR  = lift . getRandomR
  {-# INLINE getRandomR #-}
  getRandomRs = lift . getRandomRs
  {-# INLINE getRandomRs #-}

instance Foldable m => Foldable (EitherT e m) where
  foldMap f = foldMap (either mempty f) . runEitherT
  {-# INLINE foldMap #-}

instance (Functor f, MonadFree f m) => MonadFree f (EitherT e m) where
  wrap = EitherT . wrap . fmap runEitherT

instance (Monad f, Traversable f) => Traversable (EitherT e f) where
  traverse f (EitherT a) =
    EitherT <$> traverse (either (pure . Left) (fmap Right . f)) a
  {-# INLINE traverse #-}

instance MonadBase b m => MonadBase b (EitherT e m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

#if MIN_VERSION_monad_control(1,0,0)

instance MonadTransControl (EitherT e) where
  type StT (EitherT e) a = Either e a
  liftWith f = EitherT $ liftM return $ f runEitherT
  {-# INLINE liftWith #-}
  restoreT = EitherT
  {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (EitherT e m) where
  type StM (EitherT e m) a = StM m (StT (EitherT e) a)
  liftBaseWith = defaultLiftBaseWith
  {-# INLINE liftBaseWith #-}
  restoreM     = defaultRestoreM
  {-# INLINE restoreM #-}

#else

instance MonadTransControl (EitherT e) where
  newtype StT (EitherT e) a = StEitherT {unStEitherT :: Either e a}
  liftWith f = EitherT $ liftM return $ f $ liftM StEitherT . runEitherT
  {-# INLINE liftWith #-}
  restoreT = EitherT . liftM unStEitherT
  {-# INLINE restoreT #-}
 
instance MonadBaseControl b m => MonadBaseControl b (EitherT e m) where
  newtype StM (EitherT e m) a = StMEitherT { unStMEitherT :: StM m (StT (EitherT e) a) }
  liftBaseWith = defaultLiftBaseWith StMEitherT
  {-# INLINE liftBaseWith #-}
  restoreM     = defaultRestoreM unStMEitherT
  {-# INLINE restoreM #-}

#endif
