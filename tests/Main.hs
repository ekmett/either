{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Either.Validation
import Data.Monoid (Sum(..))

import Test.QuickCheck (Property, Gen, (===), (.&&.), Arbitrary (..), forAllShrink, oneof)
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)


main :: IO ()
main = defaultMain
    [ testProperty "identity" $ identity (<|>) empty genValSumInt shrinkValidation
    , testProperty "associativity" $ associativity (<|>) genValSumInt shrinkValidation
    ]

genValSumInt :: Gen (Validation (Sum Int) (Sum Int))
genValSumInt = genValidation

genValidation :: (Arbitrary a, Arbitrary b) => Gen (Validation a b)
genValidation = oneof
    [ fmap Failure arbitrary
    , fmap Success arbitrary
    ]

shrinkValidation :: (Arbitrary a, Arbitrary b) => Validation a b -> [Validation a b]
shrinkValidation (Success x) = Success `fmap` shrink x
shrinkValidation (Failure x) = Failure `fmap` shrink x

-- -- empty is a neutral element
-- empty <|> u  =  u
-- u <|> empty  =  u
-- -- (<|>) is associative
-- u <|> (v <|> w)  =  (u <|> v) <|> w

identity :: (Eq a, Show a) => (a -> a -> a) -> a -> Gen a -> (a -> [a]) -> Property
identity f i gen shr = forAllShrink gen shr $ \x ->
    f x i === x .&&. f i x === x

associativity :: (Eq a, Show a) => (a -> a -> a) -> Gen a -> (a -> [a]) -> Property
associativity f gen shr =
    forAllShrink gen shr $ \x ->
    forAllShrink gen shr $ \y ->
    forAllShrink gen shr $ \z ->
        f x (f y z) === f (f x y) z
