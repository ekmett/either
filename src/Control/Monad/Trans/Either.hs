{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Either
-- Copyright   :  (C) 2008-2013 Edward Kmett
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
  , left
  , right
  ) where

import Control.Monad.Trans.Either.Safe

import Control.Monad (liftM)
import Control.Monad.Trans.Class
import Control.Monad.Random (MonadRandom,getRandom,getRandoms,getRandomR,getRandomRs)
import Data.Functor.Bind
import Data.Functor.Plus
import Data.Semigroup

instance Monad m => Apply (EitherT e m) where
  EitherT f <.> EitherT v = EitherT $ f >>= \mf -> case mf of
    Left  e -> return (Left e)
    Right k -> v >>= \mv -> case mv of
      Left  e -> return (Left e)
      Right x -> return (Right (k x))
  {-# INLINE (<.>) #-}

instance (Monad m, Semigroup e) => Alt (EitherT e m) where
  EitherT m <!> EitherT n = EitherT $ m >>= \a -> case a of
    Left l -> liftM (\b -> case b of
      Left l' -> Left (l <> l')
      Right r -> Right r) n
    Right r -> return (Right r)
  {-# INLINE (<!>) #-}

instance Monad m => Bind (EitherT e m) where
  (>>-) = (>>=)
  {-# INLINE (>>-) #-}

instance MonadRandom m => MonadRandom (EitherT e m) where
  getRandom   = lift getRandom
  {-# INLINE getRandom #-}
  getRandoms  = lift getRandoms
  {-# INLINE getRandoms #-}
  getRandomR  = lift . getRandomR
  {-# INLINE getRandomR #-}
  getRandomRs = lift . getRandomRs
  {-# INLINE getRandomRs #-}

