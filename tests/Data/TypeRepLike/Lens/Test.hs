-----------------------------------------------------------------------------
--
-- Module      :  Data.TypeRepLike.Lens.Test
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module Data.TypeRepLike.Lens.Test where

import Control.Lens
import Data.Tagged
import Data.Typeable
import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

import Data.TypeRepLike
import Data.TypeRepLike.Lens

-----------------------------------------------------------------------------
-- Preliminaries
-----------------------------------------------------------------------------

data C1 a = C1 deriving (Show, Typeable)
data C2 a b = C2 deriving (Show, Typeable)
data C3 a b c = C3 deriving (Show, Typeable)
data C4 a b c d = C4 deriving (Show, Typeable)
data C5 a b c d e = C5 deriving (Show, Typeable)

float :: Tagged Float STypeRep
float = taggedTypeRep Proxy

int :: Tagged Int STypeRep
int = taggedTypeRep Proxy

double :: Tagged Double STypeRep
double = taggedTypeRep Proxy

char :: Tagged Char STypeRep
char = taggedTypeRep Proxy

unit :: Tagged () STypeRep
unit = taggedTypeRep Proxy

efi :: Tagged (Either Float Int) STypeRep
efi = taggedTypeRep Proxy

c1 :: (Typeable a)
    => Tagged a STypeRep
    -> Tagged (C1 a) STypeRep
c1 _ = taggedTypeRep Proxy

c2 :: (Typeable a, Typeable b)
    => Tagged a STypeRep
    -> Tagged b STypeRep
    -> Tagged (C2 a b) STypeRep
c2 _ _ = taggedTypeRep Proxy

c3 :: (Typeable a, Typeable b, Typeable c)
    => Tagged a STypeRep
    -> Tagged b STypeRep
    -> Tagged c STypeRep
    -> Tagged (C3 a b c) STypeRep
c3 _ _ _ = taggedTypeRep Proxy

c4 :: (Typeable a, Typeable b, Typeable c, Typeable d)
    => Tagged a STypeRep
    -> Tagged b STypeRep
    -> Tagged c STypeRep
    -> Tagged d STypeRep
    -> Tagged (C4 a b c d) STypeRep
c4 _ _ _ _ = taggedTypeRep Proxy

c5 :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e)
    => Tagged a STypeRep
    -> Tagged b STypeRep
    -> Tagged c STypeRep
    -> Tagged d STypeRep
    -> Tagged e STypeRep
    -> Tagged (C5 a b c d e) STypeRep
c5 _ _ _ _ _ = taggedTypeRep Proxy

-----------------------------------------------------------------------------
-- Actual tests
-----------------------------------------------------------------------------

testMiddle :: Assertion
testMiddle = (fbc, fab) @=? middle fac b where
    fbc = c2 float int
    fab = c2 double float
    fac = c2 double int
    b   = float

-----------------------------------------------------------------------------
-- Testing of nthLastTypeParam as getter
-----------------------------------------------------------------------------

testGetLastTypeParam :: Test
testGetLastTypeParam = testGroup "test lens as getter for lastTypeParam" [
    testCase "C1" $ float @=? (c1 float) ^. lastTypeParam,
    testCase "C2" $ float @=? (c2 int float) ^. lastTypeParam,
    testCase "C3" $ float @=? (c3 double int float) ^. lastTypeParam,
    testCase "C4" $ float @=? (c4 char double int float) ^. lastTypeParam,
    testCase "C5" $ float @=? (c5 unit char double int float) ^. lastTypeParam]

testGetSecondLastTypeParam :: Test
testGetSecondLastTypeParam =
    testGroup "test lens as getter for secondLastTypeParam" [
        testCase "C2" $ int @=? (c2 int float) ^. secondLastTypeParam,
        testCase "C3" $ int @=? (c3 double int float) ^. secondLastTypeParam,
        testCase "C4" $
            int @=? (c4 char double int float) ^. secondLastTypeParam,
        testCase "C5" $
            int @=? (c5 unit char double int float) ^. secondLastTypeParam]

testGetThirdLastTypeParam :: Test
testGetThirdLastTypeParam =
    testGroup "test lens as getter for thirdLastTypeParam" [
        testCase "C3" $ double @=? (c3 double int float) ^. thirdLastTypeParam,
        testCase "C4" $
            double @=? (c4 char double int float) ^. thirdLastTypeParam,
        testCase "C5" $
            double @=? (c5 unit char double int float) ^. thirdLastTypeParam]

testGetFourthLastTypeParam :: Test
testGetFourthLastTypeParam =
    testGroup "test lens as getter for fourthLastTypeParam" [
        testCase "C4" $
            char @=? (c4 char double int float) ^. fourthLastTypeParam,
        testCase "C5" $
            char @=? (c5 unit char double int float) ^. fourthLastTypeParam]

testGetFifthLastTypeParam :: Test
testGetFifthLastTypeParam =
    testGroup "test lens as getter for fifthLastTypeParam" [
        testCase "C5" $
            unit @=? (c5 unit char double int float) ^. fifthLastTypeParam]

testGetters :: Test
testGetters = testGroup "getters" [
    testGetLastTypeParam,
    testGetSecondLastTypeParam,
    testGetThirdLastTypeParam,
    testGetFourthLastTypeParam,
    testGetFifthLastTypeParam]

-----------------------------------------------------------------------------
-- Testing of nthLastTypeParam as setter
-----------------------------------------------------------------------------

testSetLastTypeParam :: Test
testSetLastTypeParam = testGroup "test lens as setter for lastTypeParam" [
    testCase "C1" $ (c1 efi) @=? (lastTypeParam .~ efi) (c1 float),
    testCase "C2" $ (c2 int efi) @=? (lastTypeParam .~ efi) (c2 int float),
    testCase "C3" $ (c3 double int efi) @=? (lastTypeParam .~ efi)
        (c3 double int float),
    testCase "C4" $ (c4 char double int efi) @=? (lastTypeParam .~ efi)
        (c4 char double int float),
    testCase "C5" $ (c5 unit char double int efi) @=? (lastTypeParam .~ efi)
        (c5 unit char double int float)]

testSetSecondLastTypeParam :: Test
testSetSecondLastTypeParam =
    testGroup "test lens as setter for secondLastTypeParam" [
        testCase "C2" $ (c2 efi float) @=? (secondLastTypeParam .~ efi)
            (c2 int float),
        testCase "C3" $ (c3 double efi float) @=? (secondLastTypeParam .~ efi)
            (c3 double int float),
        testCase "C4" $ (c4 char double efi float) @=?
            (secondLastTypeParam .~ efi) (c4 char double int float),
        testCase "C5" $ (c5 unit char double efi float) @=?
            (secondLastTypeParam .~ efi) (c5 unit char double int float)]

testSetThirdLastTypeParam :: Test
testSetThirdLastTypeParam =
    testGroup "test lens as setter for thirdLastTypeParam" [
        testCase "C3" $ (c3 efi int float) @=? (thirdLastTypeParam .~ efi)
            (c3 double int float),
        testCase "C4" $ (c4 char efi int float) @=?
            (thirdLastTypeParam .~ efi) (c4 char double int float),
        testCase "C5" $ (c5 unit char efi int float) @=?
            (thirdLastTypeParam .~ efi) (c5 unit char double int float)]

testSetFourthLastTypeParam :: Test
testSetFourthLastTypeParam =
    testGroup "test lens as setter for fourthLastTypeParam" [
        testCase "C4" $ (c4 efi double int float) @=?
            (fourthLastTypeParam .~ efi) (c4 char double int float),
        testCase "C5" $ (c5 unit efi double int float) @=?
            (fourthLastTypeParam .~ efi) (c5 unit char double int float)]

testSetFifthLastTypeParam :: Test
testSetFifthLastTypeParam =
    testGroup "test lens as setter for fifthLastTypeParam" [
        testCase "C5" $ (c5 efi char double int float) @=?
            (fifthLastTypeParam .~ efi) (c5 unit char double int float)]

testSetters :: Test
testSetters = testGroup "setters" [
    testSetLastTypeParam,
    testSetSecondLastTypeParam,
    testSetThirdLastTypeParam,
    testSetFourthLastTypeParam,
    testSetFifthLastTypeParam]

-----------------------------------------------------------------------------
-- All test cases
-----------------------------------------------------------------------------

testLenses :: Test
testLenses = testGroup "lenses" [
    testGetters,
    testSetters,
    testCase "middle" testMiddle]
