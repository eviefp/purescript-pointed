-- | This module defines conversion functions between Can, Smash, and Wedge.
-- |
-- | The examples assume that the pointed modules are imported qualified:
-- |
-- | ```purescript
-- | import Data.Pointed.Can as C
-- | import Data.Pointed.Smash as S
-- | import Data.Pointed.Wedge as W
-- | ```
module Data.Pointed.Convert where

import Prelude

import Data.Pointed.Can (Can)
import Data.Pointed.Can as C
import Data.Pointed.Smash (Smash)
import Data.Pointed.Smash as S
import Data.Pointed.Wedge (Wedge)
import Data.Pointed.Wedge as W

-- | Convert from `Can` to `Wedge`.
-- |
-- | ```purescript
-- | > canSmash (C.Non :: Can String Int)
-- | S.Non
-- |
-- | > canSmash (C.One "hello" :: Can String Int)
-- | S.Non
-- |
-- | > canSmash (C.Eno 42 :: Can String Int)
-- | S.Non
-- |
-- | > canSmash (C.Two "hello" 42 :: Can String Int)
-- | S.Two "hello" 42
-- | ```
canSmash :: forall a b. Can a b -> Smash a b
canSmash = C.can S.Non (const S.Non) (const S.Non) S.Two

-- | Convert from `Wedge` to `Can`.
-- |
-- | ```purescript
-- | > smashCan (S.Non :: Smash String Int)
-- | C.Non
-- |
-- | > smashCan (S.Two "hello" 42 :: Smash String Int)
-- | C.Two "hello" 42
-- | ```
smashCan :: forall a b. Smash a b -> Can a b
smashCan = S.smash C.Non C.Two

-- | Convert from `Can` to `Wedge`.
-- |
-- | ```purescript
-- | > canWedge (C.Non :: Can String Int)
-- | W.Non
-- |
-- | > canWedge (C.One "hello" :: Can String Int)
-- | W.One "hello"
-- |
-- | > canWedge (C.Eno 42 :: Can String Int)
-- | W.Eno 42
-- |
-- | > canWedge (C.Two "hello" 42 :: Can String Int)
-- | W.Non
-- | ```
canWedge :: forall a b. Can a b -> Wedge a b
canWedge = C.can W.Non W.One W.Eno (\_ _ -> W.Non)

-- | Convert from `Wedge` to `Can`.
-- |
-- | ```purescript
-- | > wedgeCan (W.Non :: Wedge String Int)
-- | C.Non
-- |
-- | > wedgeCan (W.One "hello" :: Wedge String Int)
-- | C.One "hello"
-- |
-- | > wedgeCan (W.Eno 42 :: Wedge String Int)
-- | C.Eno 42
-- | ```
wedgeCan :: forall a b. Wedge a b -> Can a b
wedgeCan = W.wedge C.Non C.One C.Eno

-- | Convert from `Wedge` to `Smash`, given default values for `a` and `b`.
-- |
-- | ```purescript
-- | > hulkSmash "world" 1 (W.Non :: Wedge String Int)
-- | S.Non
-- |
-- | > hulkSmash "world" 1 (W.One "hello" :: Wedge String Int)
-- | S.Two "hello" 1
-- |
-- | > hulkSmash "world" 1 (W.Eno 42 :: Wedge String Int)
-- | C.Eno "world" 42
-- | ```
hulkSmash :: forall a b. a -> b -> Wedge a b -> Smash a b
hulkSmash a b = W.wedge S.Non (flip S.Two b) (S.Two a)
