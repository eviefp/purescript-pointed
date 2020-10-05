module Data.Pointed.Smash where

import Prelude

import Control.Alternative (class Alternative, empty, (<|>))
import Control.Biapplicative (class Biapplicative)
import Control.Biapply (class Biapply)
import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldlDefault, bifoldrDefault)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Bitraversable (class Bitraversable, bisequenceDefault, bitraverse)
import Data.Foldable (class Foldable, foldlDefault, foldr, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), catMaybes)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (class Traversable, sequenceDefault)
import Data.Tuple (Tuple(..))

data Smash a b
  = Non
  | Two a b

derive instance eqSmash :: (Eq a, Eq b) => Eq (Smash a b)
derive instance ordSmash :: (Ord a, Ord b) => Ord (Smash a b)
derive instance genericSmash :: Generic (Smash a b) _
derive instance functorSmash :: Functor (Smash a)

instance semigroupSmash :: (Semigroup a, Semigroup b) => Semigroup (Smash a b) where
  append s1 s2 = case s1, s2 of
    Non      , s         -> s
    s        , Non       -> s
    Two a1 b1, Two a2 b2 -> Two (a1 <> a2) (b1 <> b2)

instance monoidSmash :: (Semigroup a, Semigroup b) => Monoid (Smash a b) where
  mempty = Non

instance bifunctorSmash :: Bifunctor Smash where
  bimap f g = case _ of
    Non     -> Non
    Two a b -> Two (f a) (g b)

instance biapplySmash :: Biapply Smash where
  biapply s1 s2 = case s1, s2 of
    Two f g, rest -> bimap f g rest
    _      , _      -> Non

instance biapplicativeSmash :: Biapplicative Smash where
  bipure = Two

instance bifoldableSmash :: Bifoldable Smash where
  bifoldMap f g = case _ of
    Non     -> mempty
    Two a b -> f a <> g b

  bifoldr f g m = bifoldrDefault f g m
  bifoldl f g m = bifoldlDefault f g m

instance bitraversableSmash :: Bitraversable Smash where
  bitraverse f g = case _ of
    Non     -> pure Non
    Two a b -> Two <$> f a <*> g b

  bisequence = bisequenceDefault

instance applySmash :: Semigroup a => Apply (Smash a) where
  apply s1 s2 = case s1, s2 of
    Two a1 f, Two a2 b -> Two (a1 <> a2) (f b)
    _       , _        -> Non

instance applicativeSmash :: Monoid a => Applicative (Smash a) where
  pure = Two mempty

instance bindSmash :: Monoid a => Bind (Smash a) where
  bind s1 f = case s1 of
    Non      -> Non
    Two a1 b -> case f b of
      Non      -> Non
      Two a2 c -> Two (a1 <> a2) c

instance monadSmash :: Monoid a => Monad (Smash a)

instance foldableSmash :: Foldable (Smash a) where
  foldMap f = bifoldMap mempty f

  foldl f m = foldlDefault f m
  foldr f m = foldrDefault f m

instance traversableSmash :: Traversable (Smash a) where
  traverse = bitraverse pure
  sequence = sequenceDefault

fromMaybe :: forall a b. Maybe (Tuple a b) -> Smash a b
fromMaybe = maybe Non (\(Tuple a b) -> Two a b)

toMaybe :: forall a b. Smash a b -> Maybe (Tuple a b)
toMaybe = smash Nothing (\a b -> Just (Tuple a b))

smash :: forall a b c. c -> (a -> b -> c) -> Smash a b -> c
smash non two = case _ of
  Non     -> non
  Two a b -> two a b

first :: forall a b. Smash a b -> Maybe a
first = smash Nothing (const <<< Just)

second :: forall a b. Smash a b -> Maybe b
second = smash Nothing (const Just)

both :: forall a b. Smash a b -> Maybe (Tuple a b)
both = smash Nothing (\a b -> Just $ Tuple a b)

isNone :: forall a b. Smash a b -> Boolean
isNone = smash true (const (const false))

isTwo :: forall a b. Smash a b -> Boolean
isTwo = smash false (const (const true))

twos :: forall a b f. Functor f => Foldable f => f (Smash a b) -> List (Tuple a b)
twos = catMaybes <<< foldr Cons Nil <<< map both

curry :: forall a b c. (Smash a b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
curry f ma mb = case ma, mb of
  Just a, Just b -> f (Two a b)
  _     , _      -> f Non

uncurry :: forall a b c. (Maybe a -> Maybe b -> Maybe c) -> Smash a b -> Maybe c
uncurry f = case _ of
  Non     -> f Nothing Nothing
  Two a b -> f (Just a) (Just b)

partition
  :: forall f t a b
   . Foldable t
  => Alternative f
  => t (Smash a b)
  -> Tuple (f a) (f b)
partition = foldr go (Tuple empty empty)
  where
    go :: Smash a b -> Tuple (f a) (f b) -> Tuple (f a) (f b)
    go c original@(Tuple fa fb) = case c of
      Non     -> original
      Two a b -> Tuple (fa <|> pure a) (fb <|> pure b)

partitionMap
  :: forall f t a b c
   . Alternative f
  => Traversable t
  => (a -> Smash b c)
  -> t a
  -> Tuple (f b) (f c)
partitionMap f = foldr go (Tuple empty empty)
  where
    go :: a -> Tuple (f b) (f c) -> Tuple (f b) (f c)
    go a original@(Tuple fb fc) = case f a of
      Non     -> original
      Two b c -> Tuple (fb <|> pure b) (fc <|> pure c)

reassocLR :: forall a b c. Smash (Smash a b) c -> Smash a (Smash b c)
reassocLR = case _ of
  Two (Two a b) c -> Two a (Two b c)
  _               -> Non

reassocRL :: forall a b c. Smash a (Smash b c) -> Smash (Smash a b) c
reassocRL = case _ of
  Two a (Two b c) -> Two (Two a b) c
  _               -> Non

swap :: forall a b. Smash a b -> Smash b a
swap = smash Non (flip Two)
