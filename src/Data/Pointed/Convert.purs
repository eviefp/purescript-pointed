module Data.Pointed.Convert where

import Prelude

import Data.Pointed.Can (Can)
import Data.Pointed.Can as C
import Data.Pointed.Smash (Smash)
import Data.Pointed.Smash as S
import Data.Pointed.Wedge (Wedge)
import Data.Pointed.Wedge as W

canSmash :: forall a b. Can a b -> Smash a b
canSmash = C.can S.Non (const S.Non) (const S.Non) S.Two

smashCan :: forall a b. Smash a b -> Can a b
smashCan = S.smash C.Non C.Two

canWedge :: forall a b. Can a b -> Wedge a b
canWedge = C.can W.Non W.One W.Eno (\_ _ -> W.Non)

wedgeCan :: forall a b. Wedge a b -> Can a b
wedgeCan = W.wedge C.Non C.One C.Eno

hulkSmash :: forall a b. a -> b -> Wedge a b -> Smash a b
hulkSmash a b = W.wedge S.Non (flip S.Two b) (S.Two a)
