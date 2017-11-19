{-# language CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Either.Combinators
-- Copyright   :  (c) 2010-2014 Gregory Crosswhite, Chris Done, Edward Kmett
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
  , maybeToLeft
  , maybeToRight
  , eitherToError
  , swapEither
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.Error.Class ( MonadError(throwError) )

-- ---------------------------------------------------------------------------
-- Functions over Either

-- |The 'isLeft' function returns 'True' iff its argument is of the form @'Left' _@.
--
-- Using @Control.Lens@:
--
-- @
-- 'isLeft' ≡ has _Left
-- @
--
-- >>> isLeft (Left 12)
-- True
--
-- >>> isLeft (Right 12)
-- False
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

-- |The 'isRight' function returns 'True' iff its argument is of the form @'Right' _@.
--
-- Using @Control.Lens@:
--
-- @
-- 'isRight' ≡ has _Right
-- @
--
-- >>> isRight (Left 12)
-- False
--
-- >>> isRight (Right 12)
-- True
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
--
-- >>> fromLeft' (Left 12)
-- 12
fromLeft' :: Either a b -> a
fromLeft' (Right _) = error "Data.Either.Combinators.fromLeft: Argument takes form 'Right _'" -- yuck
fromLeft' (Left x)  = x

-- | Extracts the element out of a 'Right' and
-- throws an error if its argument take the form @'Left' _@.
--
-- Using @Control.Lens@:
--
-- @
-- 'fromRight'' x ≡ x^?!_Right
-- @
--
-- >>> fromRight' (Right 12)
-- 12
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
--
-- >>> mapBoth (*2) (*3) (Left 4)
-- Left 8
--
-- >>> mapBoth (*2) (*3) (Right 4)
-- Right 12
mapBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f _ (Left x)  = Left (f x)
mapBoth _ f (Right x) = Right (f x)

-- | The 'mapLeft' function takes a function and applies it to an Either value
-- iff the value takes the form @'Left' _@.
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
--
-- >>> mapLeft (*2) (Left 4)
-- Left 8
--
-- >>> mapLeft (*2) (Right "hello")
-- Right "hello"
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = mapBoth f id

-- | The 'mapRight' function takes a function and applies it to an Either value
-- iff the value takes the form @'Right' _@.
--
-- Using @Data.Bifunctor@:
--
-- @
-- 'mapRight' = second
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
-- 'mapRight' = over _Right
-- @
--
-- >>> mapRight (*2) (Left "hello")
-- Left "hello"
--
-- >>> mapRight (*2) (Right 4)
-- Right 8
mapRight :: (b -> c) -> Either a b -> Either a c
mapRight = mapBoth id

-- | The 'whenLeft' function takes an 'Either' value and a function which returns a monad.
-- The monad is only executed when the given argument takes the form @'Left' _@, otherwise
-- it does nothing.
--
-- Using @Control.Lens@:
--
-- @
-- 'whenLeft' ≡ forOf_ _Left
-- @
--
-- >>> whenLeft (Left 12) print
-- 12
whenLeft :: Applicative m => Either a b -> (a -> m ()) -> m ()
whenLeft (Left x) f = f x
whenLeft _        _ = pure ()

-- | The 'whenRight' function takes an 'Either' value and a function which returns a monad.
-- The monad is only executed when the given argument takes the form @'Right' _@, otherwise
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
--
-- >>> whenRight (Right 12) print
-- 12
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
-- 'fromLeft' b ≡ 'either' 'id' ('const' b)
-- @
--
-- >>> fromLeft "hello" (Right 42)
-- "hello"
--
-- >>> fromLeft "hello" (Left "world")
-- "world"
fromLeft :: a -> Either a b -> a
fromLeft _ (Left x) = x
fromLeft x _ = x

-- | Extract the right value or a default.
--
-- @
-- 'fromRight' b ≡ 'either' ('const' b) 'id'
-- @
--
-- >>> fromRight "hello" (Right "world")
-- "world"
--
-- >>> fromRight "hello" (Left 42)
-- "hello"
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
--
-- >>> leftToMaybe (Left 12)
-- Just 12
--
-- >>> leftToMaybe (Right 12)
-- Nothing
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
--
-- >>> rightToMaybe (Left 12)
-- Nothing
--
-- >>> rightToMaybe (Right 12)
-- Just 12
rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

-- | Maybe produce a 'Left', otherwise produce a 'Right'.
--
-- >>> maybeToRight "default" (Just 12)
-- Left 12
--
-- >>> maybeToRight "default" Nothing
-- Right "default"
maybeToLeft :: b -> Maybe a -> Either a b
maybeToLeft _ (Just x) = Left x
maybeToLeft y Nothing  = Right y

-- | Maybe produce a 'Right', otherwise produce a 'Left'.
--
-- >>> maybeToRight "default" (Just 12)
-- Right 12
--
-- >>> maybeToRight "default" Nothing
-- Left "default"
maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x) = Right x
maybeToRight y Nothing  = Left y

-- | Generalize @Either e@ as @MonadError e m@.
--
-- If the argument has form @Left e@, an error is produced in the monad via
-- 'throwError'. Otherwise, the @Right a@ part is forwarded.
eitherToError :: (MonadError e m) => Either e a -> m a
eitherToError = either throwError return

-- | Swap the 'Left' and 'Right' sides of an 'Either'.
--
-- @
-- >>> swapEither (Right 3)
-- Left 3
--
-- >>> swapEither (Left "error")
-- Right "error"
-- @
swapEither :: Either e a -> Either a e
swapEither = either Right Left
{-# INLINE swapEither #-}
