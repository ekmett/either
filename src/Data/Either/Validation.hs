{-# LANGUAGE Rank2Types #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Either.Validation
-- Copyright   :  (c) 2014 Chris Allen, Edward Kmett
-- License     :  BSD-style
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Monoidal 'Validation' sibling to 'Either'.
--
-----------------------------------------------------------------------------

module Data.Either.Validation
  ( Validation(..)
  , _Success
  , _Failure
  , eitherToValidation
  , validationToEither
  , _Validation
  , vap
  , ealt
  -- combinators that leak less, but require monoid constraints
  , vapm, apm
  ) where

import Control.Applicative
import Data.Bifoldable(Bifoldable(bifoldr))
import Data.Bifunctor(Bifunctor(bimap))
import Data.Bitraversable(Bitraversable(bitraverse))
import Data.Foldable (Foldable(foldr))
import Data.Functor.Alt (Alt((<!>)))
import Data.Functor.Apply (Apply ((<.>)))
import Data.Monoid (Monoid(mappend, mempty))
import Data.Profunctor
import Data.Semigroup (Semigroup((<>)))
import Data.Traversable (Traversable(traverse))
import Prelude hiding (foldr)

-- | 'Validation' is 'Either' with a Left that is a 'Monoid'
data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Ord, Show)

instance Functor (Validation e) where
   fmap _ (Failure e) = Failure e
   fmap f (Success a) = Success (f a)

instance Semigroup e => Apply (Validation e) where
  Failure e1 <.> b = Failure $ case b of
    Failure e2 -> e1 <> e2
    Success _  -> e1
  Success _  <.> Failure e  = Failure  e
  Success f  <.> Success x  = Success (f x)

instance Semigroup e => Applicative (Validation e) where
  pure = Success
  (<*>) = (<.>)

instance Semigroup e => Alt (Validation e) where
  s@Success{} <!> _ = s
  _ <!> s@Success{} = s
  Failure m <!> Failure n = Failure (m <> n)

instance (Semigroup e, Monoid e) => Alternative (Validation e) where
  empty = Failure mempty
  (<|>) = (<!>)

instance Foldable (Validation e) where
  foldr f x (Success a) = f a x
  foldr _ x (Failure _) = x

instance Traversable (Validation e) where
  traverse f (Success a) = Success <$> f a
  traverse _ (Failure e) = pure (Failure e)

instance Bifunctor Validation where
  bimap f _ (Failure e) = Failure (f e)
  bimap _ g (Success a) = Success (g a)

instance Bifoldable Validation where
  bifoldr _ g x (Success a) = g a x
  bifoldr f _ x (Failure e) = f e x

instance Bitraversable Validation where
  bitraverse _ g (Success a) = Success <$> g a
  bitraverse f _ (Failure e) = Failure <$> f e

instance Semigroup e => Semigroup (Validation e a) where
  x@Success{} <> _ = x
  _ <> x@Success{} = x
  Failure e1 <> Failure e2 = Failure (e1 <> e2)

instance Monoid e => Monoid (Validation e a) where
  mempty = Failure mempty
  x@Success{} `mappend` _ = x
  _ `mappend` x@Success{} = x
  Failure e1 `mappend` Failure e2 = Failure (e1 `mappend` e2)

type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}

_Failure :: Prism (Validation a c) (Validation b c) a b
_Failure = prism
           (\ x -> Failure x)
           (\ x
            -> case x of
              Failure y -> Right y
              Success y -> Left (Success y))
{-# INLINE _Failure #-}

_Success :: Prism (Validation c a) (Validation c b) a b
_Success = prism
           (\ x -> Success x)
           (\ x
            -> case x of
              Failure y -> Left (Failure y)
              Success y -> Right y)
{-# INLINE _Success #-}

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}

validationToEither :: Validation e a -> Either e a
validationToEither x = case x of
  Failure e -> Left e
  Success a -> Right a
{-# INLINE validationToEither #-}

eitherToValidation :: Either e a -> Validation e a
eitherToValidation x = case x of
  Left e -> Failure e
  Right a -> Success a
{-# INLINE eitherToValidation #-}

-- | 'Validation' is isomorphic to 'Either'
_Validation :: Iso (Validation e a) (Validation g b) (Either e a) (Either g b)
_Validation = iso validationToEither eitherToValidation
{-# INLINE _Validation #-}

vap :: Semigroup m => Either m (a -> b) -> Either m a -> Either m b
vap (Left m) b = Left $ case b of
  Left n  -> m <> n
  Right{} -> m
vap Right{} (Left n) = Left n
vap (Right f) (Right a) = Right (f a)
{-# INLINE vap #-}

apm :: Monoid m => Validation m (a -> b) -> Validation m a -> Validation m b
apm (Failure m) b = Failure $ m `mappend` case b of
  Failure n  -> n
  Success{} -> mempty
apm Success{} (Failure n) = Failure n
apm (Success f) (Success a) = Success (f a)
{-# INLINE apm #-}

-- lazier version of vap that can leak less, but which requires a Monoid
vapm :: Monoid m => Either m (a -> b) -> Either m a -> Either m b
vapm (Left m) b = Left $ m `mappend` case b of
  Left n  -> n
  Right{} -> mempty
vapm Right{} (Left n) = Left n
vapm (Right f) (Right a) = Right (f a)
{-# INLINE vapm #-}

ealt :: Validation e a -> Validation e a -> Validation e a
ealt Failure{} r = r
ealt (Success a) _ = Success a
{-# INLINE ealt #-}
