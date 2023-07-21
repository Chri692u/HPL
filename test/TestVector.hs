{-# LANGUAGE TemplateHaskell #-}

module TestVector where

import Test.QuickCheck
import Math.Vector

-- Property: Vector addition is commutative.
prop_vectorAddCommutative :: Vector Int -> Vector Int -> Bool
prop_vectorAddCommutative v1 v2 = v1 +. v2 == v2 +. v1

-- Property: Vector subtraction is anti-commutative.
--prop_vectorSubtractAntiCommutative :: Vector Int -> Vector Int -> Bool
--prop_vectorSubtractAntiCommutative v1 v2 = v1 -. v2 == negate (v2 -. v1)

-- Property: Vector multiplication is distributive over addition.
prop_vectorMultDistributive :: Vector Int -> Vector Int -> Vector Int -> Bool
prop_vectorMultDistributive v1 v2 v3 = (v1 +. v2) *. v3 == v1 *. v3 +. v2 *. v3

-- Property: Dot product is commutative.
prop_vectorDotCommutative :: Vector Int -> Vector Int -> Bool
prop_vectorDotCommutative v1 v2 = dot v1 v2 == dot v2 v1

-- Property: The projection of a vector onto itself is the vector itself.
prop_vectorProjectionOnItself :: Vector Double -> Bool
prop_vectorProjectionOnItself vec = vec `projection` vec == vec

-- Property: The dot product of a vector and its projection onto another vector is the product of their norms.
prop_vectorProjectionDotNorm :: Vector Double -> Vector Double -> Property
prop_vectorProjectionDotNorm vec onto =
    let normVec = sqrt (dot vec vec)
        normOnto = sqrt (dot onto onto)
    in normOnto /= 0 ==> vec `dot` (vec `projection` onto) == normVec * normOnto

runVectorTests :: IO Bool
runVectorTests = $quickCheckAll
