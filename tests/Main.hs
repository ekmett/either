{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Data.Either.Validation
import           Data.Monoid (Sum(..))

import           Data.Functor (void)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- -- empty is a neutral element
-- empty <|> u  =  u
-- u <|> empty  =  u
-- -- (<|>) is associative
-- u <|> (v <|> w)  =  (u <|> v) <|> w

genValidation :: Gen a -> Gen b -> Gen (Validation a b)
genValidation ga gb = do
  a <- ga
  b <- gb
  Gen.choice [return $ Failure a, return $ Success b]

identity :: (Eq a, Show a) => (a -> a -> a) -> a -> Gen a -> PropertyT IO ()
identity f i gen = do
  x <- forAll gen
  f x i === x
  f i x === x

assoc :: (Eq a, Show a) => (a -> a -> a) -> Gen a -> PropertyT IO ()
assoc f gen = do
  x <- forAll gen
  y <- forAll gen
  z <- forAll gen

  let xy = f x y
      yz = f y z

  f x yz === f xy z

prop_alternative :: Property
prop_alternative = property $ do
  let genSumInt = Sum <$> Gen.int (Range.linear 0 maxBound)
      genVal = genValidation genSumInt genSumInt
  identity (<|>) empty genVal
  assoc (<|>) genVal

main :: IO ()
main =
  void $ checkParallel $ Group "Test.Either" [
      ("prop_alternative", prop_alternative)
    ]
