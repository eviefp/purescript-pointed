module Data.Pointed.Wedge where

import Prelude

import Control.Alternative (class Alternative, empty, (<|>))
import Control.Biapply (class Biapply)
import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldlDefault, bifoldrDefault)
import Data.Bifunctor (class Bifunctor)
import Data.Bitraversable (class Bitraversable, bisequenceDefault, bitraverse)
import Data.Foldable (class Foldable, foldlDefault, foldr, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), catMaybes)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequenceDefault)
import Data.Tuple (Tuple(..))

data Wedge a b
  = Non
  | One a
  | Eno b

derive instance eqWedge :: (Eq a, Eq b) => Eq (Wedge a b)
derive instance ordWedge :: (Ord a, Ord b) => Ord (Wedge a b)
derive instance genericWedge :: Generic (Wedge a b) _
derive instance functorWedge :: Functor (Wedge a)

instance semigroupWedge :: (Semigroup a, Semigroup b) => Semigroup (Wedge a b) where
  append s1 s2 = case s1, s2 of
    Non   , s      -> s
    s     , Non    -> s
    One a1, One a2 -> One (a1 <> a2)
    One _ , Eno b  -> Eno b
    Eno b , One _  -> Eno b
    Eno b1, Eno b2 -> Eno (b1 <> b2)


instance monoidWedge :: (Semigroup a, Semigroup b) => Monoid (Wedge a b) where
  mempty = Non

instance bifunctorWedge :: Bifunctor Wedge where
  bimap f g = case _ of
    Non   -> Non
    One a -> One (f a)
    Eno b -> Eno (g b)

instance biapplyWedge :: Biapply Wedge where
  biapply s1 s2 = case s1, s2 of
   One f, One a -> One (f a)
   Eno g, Eno b -> Eno (g b)
   _    , _     -> Non

instance bifoldableWedge :: Bifoldable Wedge where
  bifoldMap f g = case _ of
    Non   -> mempty
    One a -> f a
    Eno b -> g b

  bifoldr f g m = bifoldrDefault f g m
  bifoldl f g m = bifoldlDefault f g m

instance bitraversableWedge :: Bitraversable Wedge where
  bitraverse f g = case _ of
    Non     -> pure Non
    One a   -> One <$> f a
    Eno b   -> Eno <$> g b

  bisequence = bisequenceDefault

instance applyWedge :: Semigroup a => Apply (Wedge a) where
  apply s1 s2 = case s1, s2 of
    Non, _       -> Non
    _  , Non     -> Non
    One a, _     -> One a
    _    , One a -> One a
    Eno f, Eno a -> Eno (f a)

instance applicativeWedge :: Monoid a => Applicative (Wedge a) where
  pure = Eno

instance bindWedge :: Monoid a => Bind (Wedge a) where
  bind s1 f = case s1 of
    Non   -> Non
    One a -> One a
    Eno b -> f b

instance monadWedge :: Monoid a => Monad (Wedge a)

instance foldableWedge :: Foldable (Wedge a) where
  foldMap f = bifoldMap mempty f

  foldl f m = foldlDefault f m
  foldr f m = foldrDefault f m

instance traversableWedge :: Traversable (Wedge a) where
  traverse = bitraverse pure
  sequence = sequenceDefault

wedge :: forall a b c. c -> (a -> c) -> (b -> c) -> Wedge a b -> c
wedge non one eno = case _ of
  Non   -> non
  One a -> one a
  Eno b -> eno b

first :: forall a b. Wedge a b -> Maybe a
first = wedge Nothing Just (const Nothing)

second :: forall a b. Wedge a b -> Maybe b
second = wedge Nothing (const Nothing) Just

isNone :: forall a b. Wedge a b -> Boolean
isNone = wedge true (const false) (const false)

isOne :: forall a b. Wedge a b -> Boolean
isOne = wedge false (const true) (const false)

isEno :: forall a b. Wedge a b -> Boolean
isEno = wedge false (const false) (const true)

ones :: forall a b f. Functor f => Foldable f => f (Wedge a b) -> List a
ones = catMaybes <<< foldr Cons Nil <<< map first

enos :: forall a b f. Functor f => Foldable f => f (Wedge a b) -> List b
enos = catMaybes <<< foldr Cons Nil <<< map second

partition
  :: forall f t a b
   . Foldable t
  => Alternative f
  => t (Wedge a b)
  -> Tuple (f a) (f b)
partition = foldr go (Tuple empty empty)
  where
    go :: Wedge a b -> Tuple (f a) (f b) -> Tuple (f a) (f b)
    go c original@(Tuple fa fb) = case c of
      Non   -> original
      One a -> Tuple (fa <|> pure a) fb
      Eno b -> Tuple fa (fb <|> pure b)

partitionMap
  :: forall f t a b c
   . Alternative f
  => Traversable t
  => (a -> Wedge b c)
  -> t a
  -> Tuple (f b) (f c)
partitionMap f = foldr go (Tuple empty empty)
  where
    go :: a -> Tuple (f b) (f c) -> Tuple (f b) (f c)
    go a original@(Tuple fb fc) = case f a of
      Non   -> original
      One b -> Tuple (fb <|> pure b) fc
      Eno c -> Tuple fb (fc <|> pure c)
