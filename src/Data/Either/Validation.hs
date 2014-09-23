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
       )
       where

import Control.Applicative
import Data.Bifoldable(Bifoldable(bifoldr))
import Data.Bifunctor(Bifunctor(bimap))
import Data.Bitraversable(Bitraversable(bitraverse))
import Data.Foldable (Foldable(foldr))
import Data.Functor.Alt (Alt((<!>)))
import Data.Monoid (Monoid(mappend, mempty))
import Data.Semigroup (Semigroup((<>)))
import Data.Traversable (Traversable(traverse))
import Prelude hiding (foldr)

-- | 'Validation' is 'Either' with a Left that is a 'Monoid'
data Validation err a =
  Failure err
  | Success a
  deriving (Eq, Ord, Show)

instance Functor (Validation err) where
   fmap _ (Failure e) = Failure e
   fmap f (Success a) = Success (f a)

instance Monoid err => Applicative (Validation err) where
  pure = Success
  Failure e1 <*> Failure e2 = Failure (e1 `mappend` e2)
  Failure e1 <*> Success _  = Failure e1
  Success _  <*> Failure e2 = Failure e2
  Success f  <*> Success a  = Success (f a)

instance Alt (Validation err) where
  Failure _  <!> x = x
  Success a  <!> _ = Success a

instance Monoid err => Alternative (Validation err) where
  empty = Failure mempty
  (<|>) = (<!>)

instance Foldable (Validation err) where
  foldr f x (Success a) = f a x
  foldr _ x (Failure _) = x

instance Traversable (Validation err) where
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
  Failure e1 <> Failure e2 = Failure (e1 <> e2)
  Failure _  <> Success a2 = Success a2
  Success a1 <> Failure _  = Success a1
  Success a1 <> Success _  = Success a1

instance Monoid e => Monoid (Validation e a) where
  mempty = Failure mempty
  Failure e1 `mappend` Failure e2 = Failure (e1 `mappend` e2)
  Failure _  `mappend` Success a2 = Success a2
  Success a1 `mappend` Failure _  = Success a1
  Success a1 `mappend` Success _  = Success a1
