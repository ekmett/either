-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Either.Combinators
-- Copyright   :  (c) Gregory Crosswhite, Chris Done
-- License     :  BSD-style
--
-- Maintainer  :  gcross@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for probing and unwrapping values inside of Either.
--
-----------------------------------------------------------------------------

module Data.Either.Combinators
    (    isLeft
    ,    isRight
    ,    fromLeft
    ,    fromRight
    ,    fromLeft'
    ,    fromRight'
    ,    mapBoth
    ,    mapLeft
    ,    mapRight
    ,    eitherM
    ,    whenLeft
    ,    whenRight
    ,    unlessLeft
    ,    unlessRight
    ,    leftToMaybe
    ,    rightToMaybe
    ) where

-- ---------------------------------------------------------------------------
-- Functions over Either

-- |The 'isLeft' function returns 'True' iff its argument is of the form @Left _@.
isLeft           :: Either a b -> Bool
isLeft (Left _)  = True
isLeft _         = False

-- |The 'isRight' function returns 'True' iff its argument is of the form @Right _@.
isRight            :: Either a b -> Bool
isRight (Right _)  = True
isRight _          = False

-- | Extracts the element out of a 'Left' and
-- throws an error if its argument take the form  @Right _@.
fromLeft'           :: Either a b -> a
fromLeft' (Right _) = error "Either.Unwrap.fromLeft: Argument takes form 'Right _'" -- yuck
fromLeft' (Left x)  = x

-- | Extracts the element out of a 'Right' and
-- throws an error if its argument take the form  @Left _@.
fromRight'           :: Either a b -> b
fromRight' (Left _)  = error "Either.Unwrap.fromRight: Argument takes form 'Left _'" -- yuck
fromRight' (Right x) = x

-- | The 'mapBoth' function takes two functions and applies the first if iff the value
-- takes the form 'Left _' and the second if the value takes the form 'Right _'.
mapBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f _ (Left x) = Left (f x)
mapBoth _ f (Right x) = Right (f x)

-- | The 'mapLeft' function takes a function and applies it to an Either value
-- iff the value takes the form 'Left _'.
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft = (`mapBoth` id)

-- | The 'mapLeft' function takes a function and applies it to an Either value
-- iff the value takes the form 'Left _'.
mapRight :: (b -> c) -> Either a b -> Either a c
mapRight = (id `mapBoth`)

-- | The 'eitherM' function takes an 'Either' value and two functions which return monads.
-- If the argument takes the form @Left _@ then the element within is passed to the first
-- function, otherwise the element within is passed to the second function.
eitherM               :: Monad m => Either a b -> (a -> m c) -> (b -> m c) -> m c
eitherM (Left x)  f _ = f x
eitherM (Right x) _ f = f x

-- | The 'whenLeft' function takes an 'Either' value and a function which returns a monad.
-- The monad is only executed when the given argument takes the form @Left _@, otherwise
-- it does nothing.
whenLeft            :: Monad m => Either a b -> (a -> m ()) -> m ()
whenLeft (Left x) f = f x
whenLeft _ _        = return ()

-- | The 'whenLeft' function takes an 'Either' value and a function which returns a monad.
-- The monad is only executed when the given argument takes the form @Right _@, otherwise
-- it does nothing.
whenRight             :: Monad m => Either a b -> (b -> m ()) -> m ()
whenRight (Right x) f = f x
whenRight _ _         = return ()

-- | A synonym of 'whenRight'.
unlessLeft :: Monad m => Either a b -> (b -> m ()) -> m ()
unlessLeft = whenRight

-- | A synonym of 'whenLeft'.
unlessRight :: Monad m => Either a b -> (a -> m ()) -> m ()
unlessRight = whenLeft

-- | Extract the left value or a default.
fromLeft :: a -> Either a b -> a
fromLeft _ (Left x) = x
fromLeft x _ = x

-- | Extract the right value or a default.
fromRight :: b -> Either a b -> b
fromRight _ (Right x) = x
fromRight x _ = x

-- | Maybe get the left side of an Either.
leftToMaybe :: Either a b -> Maybe a
leftToMaybe = either Just (const Nothing)

-- | Maybe get the right side of an Either.
rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just
