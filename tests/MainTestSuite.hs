-----------------------------------------------------------------------------
--
-- Module      :  MainTestSuite
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Tests for typereplike-lens
--
-----------------------------------------------------------------------------

module Main (
    main
 ) where

import Test.Framework

import Data.TypeRepLike.Lens.Test

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [testLenses]
