-----------------------------------------------------------------------------
--
-- Module      :  Data.TypeRepLike.TypeRepLike
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | 'Lens'es on 'TypeRepLike's
--
-----------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
module Data.TypeRepLike.Lens (
    -- * Lenses
    TypeLens,
    lastTypeParam,
    secondLastTypeParam,
    thirdLastTypeParam,
    fourthLastTypeParam,
    fifthLastTypeParam,
    -- * Other combinators
    middle
) where

import Control.Lens
import Control.Monad.Identity
import Data.Sequence (Seq, (|>))
import Data.Tagged
import Data.Typeable

import qualified Data.Sequence as Seq

import Data.Peano.Extras
import Data.TypeRepLike.TypeArity
import Data.TypeRepLike.TypeRepLike

-----------------------------------------------------------------------------
-- Miscellaneous utility methods for 'Sequence's. Don't exactly belong here.
-----------------------------------------------------------------------------

-- | 'Seq.index' nth from last element of a 'Sequence'
revIndex :: Seq a -> Int -> a
revIndex as n = Seq.index as $ (Seq.length as) - (1 + n)

-- | 'Seq.update's nth from last element of a 'Sequence'
revUpdate :: Int -> a -> Seq a -> Seq a
revUpdate n a as = Seq.update ((Seq.length as) - (succ n)) a as

-- | 'Seq.take' excluding the last n elements
takeAllBut :: Int -> Seq a -> Seq a
takeAllBut n as = Seq.take ((Seq.length as) - n) as

-----------------------------------------------------------------------------
-- These functions are called unsafe because they manipulate un'Tagged'
-- versions of 'TypeRepLike's; used incorrectly in conjunction with things
-- like 'taggedCast' they /will/ cause a crash. Not exported.
-----------------------------------------------------------------------------

-- | @unsafeNth n@ is a 'Lens' to the @n@th-to-last type parameter of @r@
unsafeRevNth :: (TypeRepLike r con, Functor f) => Int -> (r -> f r) -> r -> f r
unsafeRevNth n f r = fmap setNth (f nth) where
    (c, rs) = likeSplitTyConApp r
    nth = revIndex rs n
    setNth nth' = likeMkTyConApp c $ revUpdate n nth' rs

unsafeCon :: (TypeRepLike r con, Functor f) => (con -> f con) -> r -> f r
unsafeCon f r = fmap setCon $ f c where
    (c, rs) = likeSplitTyConApp r
    setCon c' = likeMkTyConApp c' rs

-----------------------------------------------------------------------------
-- Lenses for manipulating 'Tagged' versions of 'TypeRepLike's
-----------------------------------------------------------------------------

-- | A 'Lens' that works on 'Tagged' 'TypeRepLike's (e.g. 'STypeRep'). The
--   manipulation of the 'TypeRepLike' at the value level matches the
--   type-level change. (However, note that the correctness is only tested,
--   not a consequence of how the code itself is written.) See
--   <https://gist.github.com/glguy/74960a3f1531b64a201b Lens Cookbook> for
--   an explanation of the type.
type TypeLens s t a b = forall r con f. (TypeRepLike r con, Functor f) =>
    (Tagged a r -> f (Tagged b r)) -> Tagged s r -> f (Tagged t r)

-- | 'g' is the type constructor used in 's' and 't'. Of course, nothing
--   in this function's definition actually enforces that, hence "unsafe".
unsafeTaggedFirstTypeParam :: (Nat (TypeArity g)) =>
    proxy g -> TypeLens s t a b
unsafeTaggedFirstTypeParam g f (Tagged c) = fmap (Tagged . c') (f nth) where
    n = pred $ typeArity g
    c' (Tagged a') = unsafeRevNth n .~ a' $ c
    nth = Tagged $ revIndex (snd $ likeSplitTyConApp c) n

-- | 'Lens' to last type parameter of 'g'
lastTypeParam :: forall g a a'. (Nat (TypeArity g)) =>
    TypeLens (g a) (g a') a a'
lastTypeParam = unsafeTaggedFirstTypeParam (Proxy :: Proxy g)

-- | 'Lens' to second-last type parameter of 'g'
secondLastTypeParam :: forall g a a' b. (Nat (TypeArity g)) =>
    TypeLens (g a b) (g a' b) a a'
secondLastTypeParam = unsafeTaggedFirstTypeParam (Proxy :: Proxy g)

-- | 'Lens' to third-last type parameter of 'g'
thirdLastTypeParam :: forall g a a' b c. (Nat (TypeArity g)) =>
    TypeLens (g a b c) (g a' b c) a a'
thirdLastTypeParam = unsafeTaggedFirstTypeParam (Proxy :: Proxy g)

-- | 'Lens' to fourth-last type parameter of 'g'
fourthLastTypeParam :: forall g a a' b c d. (Nat (TypeArity g)) =>
    TypeLens (g a b c d) (g a' b c d) a a'
fourthLastTypeParam = unsafeTaggedFirstTypeParam (Proxy :: Proxy g)

-- | 'Lens' to fifth-last type parameter of 'g'
fifthLastTypeParam :: forall g a a' b c d e. (Nat (TypeArity g)) =>
    TypeLens (g a b c d e) (g a' b c d e) a a'
fifthLastTypeParam = unsafeTaggedFirstTypeParam (Proxy :: Proxy g)

-----------------------------------------------------------------------------
-- Combinators built on top of above Lenses
-----------------------------------------------------------------------------

-- | The reverse of a 'Control.Category..' at the type level
middle :: (TypeRepLike r con, Nat (TypeArity f), Nat (TypeArity (f a)))
    => Tagged (f a c) r
    -- ^ The composed type
    -> Tagged b r
    -- ^ The "middle" type
    -> (Tagged (f b c) r, Tagged (f a b) r)
    -- ^ The left and right arguments to the compose operation
middle fac b = (fbc, fab) where
    fbc = secondLastTypeParam .~ b $ fac
    fab = lastTypeParam .~ b $ fac
