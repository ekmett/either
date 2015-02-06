{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}

module Control.Monad.Trans.Either.Class where

import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Trans.Either (EitherT)
import qualified Control.Monad.Trans.Either as E

class (Monad m) => MonadEither l m | m -> l where
    hoistEither :: Either l r -> m r

instance (Monad m) => MonadEither l (EitherT l m) where
    hoistEither = E.hoistEither

instance (MonadTrans t, MonadEither l (t m)) => MonadEither l (EitherT l (t m)) where
    hoistEither = lift . hoistEither
