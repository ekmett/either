-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Either.Combinators
-- Copyright   :  (c) Gregory Crosswhite, Chris Done, Edward Kmett
-- License     :  BSD-style
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for probing and unwrapping values inside of 'Either'.
--
-- Most of these combinators are provided for pedagogical purposes and exist
-- in more general forms in other libraries. To that end alternative definitions
-- are supplied below.
--
-----------------------------------------------------------------------------

module Data.Either.Combinators
  ( isLeft
  , isRight
  , fromLeft
  , fromRight
  , fromLeft'
  , fromRight'
  , mapBoth
  , mapLeft
  , mapRight
  , whenLeft
  , whenRight
  , unlessLeft
  , unlessRight
  , leftToMaybe
  , rightToMaybe
  ) where

import Control.Applicative

-- ---------------------------------------------------------------------------
-- Functions over Either

-- |The 'isLeft' function returns 'True' iff its argument is of the form @Left _@.
--
-- Using @Control.Lens@:
--
-- @
-- 'isLeft' ≡ has _Left
-- @
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

-- |The 'isRight' function returns 'True' iff its argument is of the form @Right _@.
--
-- Using @Control.Lens@:
--
-- @
-- 'isRight' ≡ has _Right
-- @
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

-- | Extracts the element out of a 'Left' and
-- throws an error if its argument take the form  @'Right' _@.
--
-- Using @Control.Lens@:
--
-- @
-- 'fromLeft'' x ≡ x^?!_Left
-- @
fromLeft' :: Either a b -> a
fromLeft' (Right _) = error "Data.Either.Combinators.fromLeft: Argument takes form 'Right _'" -- yuck
fromLeft' (Left x)  = x

-- | Extracts the element out of a 'Right' and
-- throws an error if its argument take the form  @'Left' _@.
--
-- Using @Control.Lens@:
--
-- @
-- 'fromRight'' x ≡ x^?!_Right
-- @
fromRight' :: Either a b -> b
fromRight' (Left _)  = error "Data.Either.Combinators.fromRight: Argument takes form 'Left _'" -- yuck
fromRight' (Right x) = x

-- | The 'mapBoth' function takes two functions and applies the first if iff the value
-- takes the form @'Left' _@ and the second if the value takes the form @'Right' _@.
--
-- Using @Data.Bifunctor@:
--
-- @
-- 'mapBoth' = bimap
-- @
--
-- Using @Control.Arrow@:
--
-- @
-- 'mapBoth' = ('Control.Arrow.+++')
-- @
mapBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f _ (Left x)  = Left (f x)
mapBoth _ f (Right x) = Right (f x)

-- | The 'mapLeft' function takes a function and applies it to an Either value
-- iff the value takes the form 'Left _'.
--
-- Using @Data.Bifunctor@:
--
-- @
-- 'mapLeft' = first
-- @
--
-- Using @Control.Arrow@:
--
-- @
-- 'mapLeft' = ('Control.Arrow.left')
-- @
--
-- Using @Control.Lens@:
--
-- @
-- 'mapLeft' = over _Left
-- @
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = mapBoth f id

-- | The 'mapLeft' function takes a function and applies it to an Either value
-- iff the value takes the form 'Left _'.
--
-- Using @Data.Bifunctor@:
--
-- @
-- 'mapRight' = first
-- @
--
-- Using @Control.Arrow@:
--
-- @
-- 'mapRight' = ('Control.Arrow.right')
-- @
--
-- Using @Control.Lens@:
--
-- @
-- 'mapRight' = 'over' '_Right'
-- @
mapRight :: (b -> c) -> Either a b -> Either a c
mapRight = mapBoth id

-- | The 'whenLeft' function takes an 'Either' value and a function which returns a monad.
-- The monad is only executed when the given argument takes the form @Left _@, otherwise
-- it does nothing.
--
-- Using @Control.Lens@:
--
-- @
-- 'whenLeft' ≡ forOf_ _Left
-- @
whenLeft :: Applicative m => Either a b -> (a -> m ()) -> m ()
whenLeft (Left x) f = f x
whenLeft _        _ = pure ()

-- | The 'whenLeft' function takes an 'Either' value and a function which returns a monad.
-- The monad is only executed when the given argument takes the form @Right _@, otherwise
-- it does nothing.
--
-- Using @Data.Foldable@:
--
-- @
-- 'whenRight' ≡ 'forM_'
-- @
--
-- Using @Control.Lens@:
--
-- @
-- 'whenRight' ≡ forOf_ _Right
-- @
whenRight :: Applicative m => Either a b -> (b -> m ()) -> m ()
whenRight (Right x) f = f x
whenRight _         _ = pure ()

-- | A synonym of 'whenRight'.
unlessLeft :: Applicative m => Either a b -> (b -> m ()) -> m ()
unlessLeft = whenRight

-- | A synonym of 'whenLeft'.
unlessRight :: Applicative m => Either a b -> (a -> m ()) -> m ()
unlessRight = whenLeft

-- | Extract the left value or a default.
--
-- @
-- 'fromLeft' ≡ 'either' 'id'
-- @
fromLeft :: a -> Either a b -> a
fromLeft _ (Left x) = x
fromLeft x _ = x

-- | Extract the right value or a default.
--
-- @
-- 'fromRight' b ≡ 'either' b 'id'
-- @
fromRight :: b -> Either a b -> b
fromRight _ (Right x) = x
fromRight x _ = x

-- | Maybe get the 'Left' side of an 'Either'.
--
-- @
-- 'leftToMaybe' ≡ 'either' 'Just' ('const' 'Nothing')
-- @
--
-- Using @Control.Lens@:
--
-- @
-- 'leftToMaybe' ≡ preview _Left
-- 'leftToMaybe' x ≡ x^?_Left
-- @
leftToMaybe :: Either a b -> Maybe a
leftToMaybe = either Just (const Nothing)

-- | Maybe get the 'Right' side of an 'Either'.
--
-- @
-- 'rightToMaybe' ≡ 'either' ('const' 'Nothing') 'Just'
-- @
--
-- Using @Control.Lens@:
--
-- @
-- 'rightToMaybe' ≡ preview _Right
-- 'rightToMaybe' x ≡ x^?_Right
-- @
rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just
