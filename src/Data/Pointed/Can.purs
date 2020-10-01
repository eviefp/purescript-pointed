-- | TODO: Module docs.
module Data.Pointed.Can where

import Prelude

import Control.Alternative (class Alternative, empty, (<|>))
import Control.Biapplicative (class Biapplicative)
import Control.Biapply (class Biapply)
import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldlDefault, bifoldrDefault)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Bitraversable (class Bitraversable, bisequenceDefault, bitraverse)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldlDefault, foldr, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), catMaybes)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequenceDefault)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))

-- | TODO: Add description.
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

both :: forall a b. Can a b -> Maybe (Tuple a b)
both = can Nothing (const Nothing) (const Nothing) (\a -> Just <<< Tuple a)

isNone :: forall a b. Can a b -> Boolean
isNone = can true (const false) (const false) (const $ const false)

isOne :: forall a b. Can a b -> Boolean
isOne = can false (const true) (const false) (const $ const false)

isEno :: forall a b. Can a b -> Boolean
isEno = can false (const false) (const true) (const $ const false)

isTwo :: forall a b. Can a b -> Boolean
isTwo = can false (const false) (const false) (const $ const true)

ones :: forall a b f. Functor f => Foldable f => f (Can a b) -> List a
ones = catMaybes <<< foldr Cons Nil <<< map first

enos :: forall a b f. Functor f => Foldable f => f (Can a b) -> List b
enos = catMaybes <<< foldr Cons Nil <<< map second

twos :: forall a b f. Functor f => Foldable f => f (Can a b) -> List (Tuple a b)
twos = catMaybes <<< foldr Cons Nil <<< map both

curry :: forall a b c. (Can a b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
curry f ma mb = case ma /\ mb of
  Nothing /\ Nothing -> f Non
  Just a  /\ Nothing -> f (One a)
  Nothing /\ Just b  -> f (Eno b)
  Just a  /\ Just b  -> f (Two a b)

uncurry :: forall a b c. (Maybe a -> Maybe b -> Maybe c) -> Can a b -> Maybe c
uncurry f = case _ of
  Non     -> f Nothing Nothing
  One a   -> f (Just a) Nothing
  Eno b   -> f Nothing (Just b)
  Two a b -> f (Just a) (Just b)

partition
  :: forall f t a b
   . Foldable t
  => Alternative f
  => t (Can a b)
  -> Tuple (f a) (f b)
partition = foldr go (Tuple empty empty)
  where
    go :: Can a b -> Tuple (f a) (f b) -> Tuple (f a) (f b)
    go c original@(Tuple fa fb) = case c of
      Non     -> original
      One a   -> Tuple (fa <|> pure a) fb
      Eno b   -> Tuple fa (fb <|> pure b)
      Two a b -> Tuple (fa <|> pure a) (fb <|> pure b)

mapCans
  :: forall f t a b c
   . Alternative f
  => Traversable t
  => (a -> Can b c)
  -> t a
  -> Tuple (f b) (f c)
mapCans f = foldr go (Tuple empty empty)
  where
    go :: a -> Tuple (f b) (f c) -> Tuple (f b) (f c)
    go a original@(Tuple fb fc) = case f a of
      Non     -> original
      One b   -> Tuple (fb <|> pure b) fc
      Eno c   -> Tuple fb (fc <|> pure c)
      Two b c -> Tuple (fb <|> pure b) (fc <|> pure c)

distribute :: forall a b c. Can (Tuple a b) c -> Tuple (Can a c) (Can b c)
distribute = case _ of
  Non               -> Tuple Non Non
  One (Tuple a b)   -> Tuple (One a) (One b)
  Eno c             -> Tuple (Eno c) (Eno c)
  Two (Tuple a b) c -> Tuple (Two a c) (Two b c)

codistribute :: forall a b c. Either (Can a c) (Can b c) -> Can (Either a b) c
codistribute =
    can Non (One <<< Left ) Eno (\a c -> Two (Left a) c)
    `either`
    can Non (One <<< Right) Eno (\b c -> Two (Right b) c)

reassocLR :: forall a b c. Can (Can a b) c -> Can a (Can b c)
reassocLR = case _ of
  Non -> Non
  One Non -> Eno Non
  One (One a) -> One a
  One (Eno b) -> Eno (One b)
  One (Two a b) -> Two a (One b)
  Eno c -> Eno (Eno c)
  Two Non c -> Eno (Eno c)
  Two (One a) c -> Two a (Eno c)
  Two (Eno b) c -> Eno (Two b c)
  Two (Two a b) c -> Two a (Two b c)

reassocRL :: forall a b c. Can a (Can b c) -> Can (Can a b) c
reassocRL = case _ of
  Non             -> Non
  One a           -> One (One a)
  Eno Non         -> One Non
  Eno (One b)     -> One (Eno b)
  Eno (Eno c)     -> Eno c
  Eno (Two b c)   -> Two (Eno b) c
  Two a Non       -> One (One a)
  Two a (One b)   -> One (Two a b)
  Two a (Eno c)   -> Two (One a) c
  Two a (Two b c) -> Two (Two a b) c

swap :: forall a b. Can a b -> Can b a
swap = can Non Eno One (flip Two)
