-- | This module defines distributivity laws between Can, Smash, and Wedge.
-- |
-- | The examples assume that the pointed modules are imported qualified:
-- |
-- | ```purescript
-- | import Data.Pointed.Can as C
-- | import Data.Pointed.Smash as S
-- | import Data.Pointed.Wedge as W
-- | ```
module Data.Pointed.Distributivity where

import Prelude

import Data.Bifunctor (bimap)
import Data.Pointed.Can (Can)
import Data.Pointed.Can as C
import Data.Pointed.Smash (Smash)
import Data.Pointed.Smash as S
import Data.Pointed.Wedge (Wedge)
import Data.Pointed.Wedge as W

-- | `smashWedge` returns `W.Non` in all cases except:
-- |
-- | - `S.Two (W.One a) c`, in which case it returns `W.One (S.Two a c)`
-- | - `S.Two (W.Eno b) c`, in which case it returns `W.Eno (S.Two b c)`
smashWedge :: forall a b c. Smash (Wedge a b) c -> Wedge (Smash a c) (Smash b c)
smashWedge = case _ of
  S.Two (W.One a) c -> W.One (S.Two a c)
  S.Two (W.Eno b) c -> W.Eno (S.Two b c)
  _                 -> W.Non

-- | `wedgeSmash` returns `S.Non` in all cases except:
-- |
-- | - `W.One (S.Two a c)`, in which case it returns `S.Two (W.One a) c`
-- | - `W.Eno (S.Two b c)`, in which case it returns `S.Two (W.Eno b) c`
wedgeSmash :: forall a b c. Wedge (Smash a c) (Smash b c) -> Smash (Wedge a b) c
wedgeSmash = case _ of
  W.One (S.Two a c) -> S.Two (W.One a) c
  W.Eno (S.Two b c) -> S.Two (W.Eno b) c
  _                 -> S.Non

-- | `smashCan` returns `C.Non` unless its input is `S.Two can c`, in which case,
-- | depending on `can`:
-- |
-- | - `C.Non` goes to `C.Non`
-- | - `C.One a` goes to `C.One (S.Two a c)`
-- | - `C.Eno b` goes to `C.Eno (S.Two b c)`
-- | - `C.Two a b` goes to `C.Two (S.Two a c) (S.Two b c)`
smashCan :: forall a b c. Smash (Can a b) c -> Can (Smash a c) (Smash b c)
smashCan = case _ of
  S.Two can c -> bimap (flip S.Two c) (flip S.Two c) can
  _           -> C.Non

-- | `canSmash` returns `S.Non` except when:
-- |
-- | `C.One (S.Two a c)` goes to `S.Two (C.One a) c`
-- | `C.Eno (S.Two b c)` goes to `S.Two (C.Eno b) c`
-- | `C.Two (S.Two a c)` goes to `S.Two (C.Two a b) c`
canSmash :: forall a b c. Can (Smash a c) (Smash b c) -> Smash (Can a b) c
canSmash = case _ of
  C.One (S.Two a c)             -> S.Two (C.One a) c
  C.Eno (S.Two b c)             -> S.Two (C.Eno b) c
  C.Two (S.Two a c) (S.Two b _) -> S.Two (C.Two a b) c
  _                             -> S.Non

