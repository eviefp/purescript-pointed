module Data.Pointed.Can
  ( Can(..)
  ) where

import Prelude

import Control.Biapplicative (class Biapplicative)
import Control.Biapply (class Biapply)
import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldlDefault, bifoldrDefault)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Bitraversable (class Bitraversable, bisequenceDefault, bitraverse)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequenceDefault)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (class Unfoldable, unfoldr)

data Can a b
  = Non
  | One a
  | Eno b
  | Two a b

derive instance eqCan :: (Eq a, Eq b) => Eq (Can a b)
derive instance ordCan :: (Ord a, Ord b) => Ord (Can a b)
derive instance genericCan :: Generic (Can a b) _
derive instance functorCan :: Functor (Can a)

instance semigroupCan :: (Semigroup a, Semigroup b) => Semigroup (Can a b) where
  append c1 c2 = case c1 /\ c2 of
    Non     /\ b       -> b
    b       /\ Non     -> b
    One a   /\ One b   -> One (a <> b)
    One a   /\ Eno b   -> Two a b
    One a   /\ Two b c -> Two (a <> b) c
    Eno a   /\ Eno b   -> Eno (a <> b)
    Eno b   /\ One a   -> Two a b
    Eno b   /\ Two a c -> Two a (b <> c)
    Two a b /\ Two c d -> Two (a <> c) (b <> d)
    Two a b /\ One c   -> Two (a <> c) b
    Two a b /\ Eno c   -> Two a (b <> c)

instance monoidCan :: (Semigroup a, Semigroup b) => Monoid (Can a b) where
  mempty = Non

instance bifunctorCan :: Bifunctor Can where
  bimap f g = case _ of
    Non     -> Non
    One a   -> One (f a)
    Eno b   -> Eno (g b)
    Two a b -> Two (f a) (g b)

instance biApplyCan :: Biapply Can where
  biapply c1 c2 = case c1 /\ c2 of
    One f   /\ One a -> One (f a)
    Eno g   /\ Eno b -> Eno (g b)
    Two f g /\ rest  -> bimap f g rest
    _                -> Non

instance biApplicativeCan :: Biapplicative Can where
  bipure = Two

instance bifoldableCan :: Bifoldable Can where
  bifoldMap f g = case _ of
    Non     -> mempty
    One a   -> f a
    Eno b   -> g b
    Two a b -> f a <> g b

  bifoldr f g m = bifoldrDefault f g m
  bifoldl f g m = bifoldlDefault f g m

instance bitraversableCan :: Bitraversable Can where
  bitraverse f g = case _ of
    Non     -> pure Non
    One a   -> One <$> f a
    Eno b   -> Eno <$> g b
    Two a b -> Two <$> f a <*> g b

  bisequence = bisequenceDefault

instance applyCan :: Semigroup a => Apply (Can a) where
  apply c1 c2 = case c1 /\ c2 of
    _       /\ Non     -> Non
    Non     /\ _       -> Non
    One a   /\ _       -> One a
    Eno _   /\ One b   -> One b
    Eno f   /\ Eno a   -> Eno (f a)
    Eno f   /\ Two a b -> Two a (f b)
    Two a _ /\ One b   -> One (a <> b)
    Two a f /\ Eno b   -> Two a (f b)
    Two a f /\ Two b c -> Two (a <> b) (f c)

instance applicativeCan :: Semigroup a => Applicative (Can a) where
  pure = Eno

instance bindCan :: Semigroup a => Bind (Can a) where
  bind c1 f = case c1 of
    Non     -> Non
    One a   -> One a
    Eno b   -> f b
    Two a b -> case f b of
      Non     -> Non
      One c   -> One (a <> c)
      Eno c   -> Eno c
      Two c d -> Two (a <> c) d

instance monadCan :: Semigroup a => Monad (Can a)

instance foldableCan :: Foldable (Can a) where
  foldMap f = bifoldMap mempty f
  foldl f m = foldlDefault f m
  foldr f m = foldrDefault f m

instance traversableCan :: Traversable (Can a) where
  traverse = bitraverse pure
  sequence = sequenceDefault

can :: forall a b c. c -> (a -> c) -> (b -> c) -> (a -> b -> c) -> Can a b -> c
can non one eno two = case _ of
  Non     -> non
  One a   -> one a
  Eno b   -> eno b
  Two a b -> two a b

first :: forall a b. Can a b -> Maybe a
first = can Nothing Just (const Nothing) (const <<< Just)

second :: forall a b. Can a b -> Maybe b
second = can Nothing (const Nothing) Just (const Just)

isNone :: forall a b. Can a b -> Boolean
isNone = can true (const false) (const false) (const $ const false)

isOne :: forall a b. Can a b -> Boolean
isOne = can false (const true) (const false) (const $ const false)

isEno :: forall a b. Can a b -> Boolean
isEno = can false (const false) (const true) (const $ const false)

isTwo :: forall a b. Can a b -> Boolean
isTwo = can false (const false) (const false) (const $ const true)

ones :: forall a b f g. Foldable f => Unfoldable g => f (Can a b) -> g a
ones = unfoldr ?wat
