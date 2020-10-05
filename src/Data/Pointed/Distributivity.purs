module Data.Pointed.Distributivity where

import Prelude

import Data.Bifunctor (bimap)
import Data.Pointed.Can (Can)
import Data.Pointed.Can as C
import Data.Pointed.Smash (Smash)
import Data.Pointed.Smash as S
import Data.Pointed.Wedge (Wedge)
import Data.Pointed.Wedge as W
import Data.Tuple (Tuple(..))

smashWedge :: forall a b c. Smash (Wedge a b) c -> Wedge (Smash a c) (Smash b c)
smashWedge = case _ of
  S.Two (W.One a) c -> W.One (S.Two a c)
  S.Two (W.Eno b) c -> W.Eno (S.Two b c)
  _                 -> W.Non

wedgeSmash :: forall a b c. Wedge (Smash a c) (Smash b c) -> Smash (Wedge a b) c
wedgeSmash = case _ of
  W.One (S.Two a c) -> S.Two (W.One a) c
  W.Eno (S.Two b c) -> S.Two (W.Eno b) c
  _                 -> S.Non

smashTuple :: forall a b c. Smash (Tuple a b) c -> Tuple (Smash a c) (Smash b c)
smashTuple =
    S.smash
        (Tuple S.Non S.Non)
        (\(Tuple a b) c -> Tuple (S.Two a c) (S.Two b c))

tupleSmash :: forall a b c. Tuple (Smash a c) (Smash b c) -> Smash (Tuple a b) c
tupleSmash = case _ of
  Tuple (S.Two a c) (S.Two b _) -> S.Two (Tuple a b) c
  _                             -> S.Non

smashCan :: forall a b c. Smash (Can a b) c -> Can (Smash a c) (Smash b c)
smashCan = case _ of
  S.Two can c -> bimap (flip S.Two c) (flip S.Two c) can
  _           -> C.Non

canSmash :: forall a b c. Can (Smash a c) (Smash b c) -> Smash (Can a b) c
canSmash = case _ of
  C.One (S.Two a c)             -> S.Two (C.One a) c
  C.Eno (S.Two b c)             -> S.Two (C.Eno b) c
  C.Two (S.Two a c) (S.Two b _) -> S.Two (C.Two a b) c
  _                             -> S.Non

