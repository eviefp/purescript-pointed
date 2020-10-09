-- | This module defines the `Smash` type, along with instances and functions that
-- | help use this type. Generally, `Smash` is used when the data is isomorphic
-- | to `Maybe (Tuple a b)`.
-- |
-- | We can map over both arguments using `bimap`:
-- |
-- | ```purescript
-- | > bimap show (_ + 10) (Non :: Smash Int Int)
-- | Non
-- |
-- | > bimap show (_ + 10) (Two 42 1 :: Smash Int Int)
-- | Two "42" 11
-- | ```
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

-- | `Smash` `a` `b` can hold either no values or an `a` and a `b`.  The type is
-- | isomorphic with `Maybe (Tuple a b)`.
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

-- | Constructs a `Smash` given an isomorphic representation. Note that this is
-- | the precise inverse of `toMaybe`.
-- |
-- | ```purescript
-- | > fromMaybe Nothing :: Smash String Int
-- | Non
-- |
-- | > fromMaybe (Just (Tuple "hello" 42)) :: Smash String Int
-- | Two "hello" 42
-- | ```
fromMaybe :: forall a b. Maybe (Tuple a b) -> Smash a b
fromMaybe = maybe Non (\(Tuple a b) -> Two a b)

-- | Destructs a `Smash` from an isomorphic representation. Note that this is
-- | the precise inverse of `fromMaybe`.
-- |
-- | ```purescript
-- | > toMaybe Non :: Smash String Int
-- | Nothing
-- |
-- | > toMaybe (Two "hello" 42 :: Smash String Int)
-- | Just (Tuple "hello" 42)
-- | ```
toMaybe :: forall a b. Smash a b -> Maybe (Tuple a b)
toMaybe = smash Nothing (\a b -> Just (Tuple a b))

-- | `Smash` catamorphism (fold). Takes an input for each possible constructor and
-- | translates it to `c`.
-- |
-- | For example, we can go from some `s :: Smash a b` to a
-- | `Maybe (Tuple a b)` using:
-- |
-- | ```purescript
-- | > smash Nothing (\a b -> Just (Tuple a b)) (Non :: Smash String Int)
-- | Nothing
-- |
-- | > smash Nothing (\a b -> Just (Tuple a b)) (Two "hello" 43 :: Smash String Int)
-- | Just (Tuple "hello" 42)
-- | ```
smash :: forall a b c. c -> (a -> b -> c) -> Smash a b -> c
smash non two = case _ of
  Non     -> non
  Two a b -> two a b

-- | Grabs an `a` if it exists, otherwise returns nothing.
-- |
-- | ```purescript
-- | > first (Non :: Smash String Int)
-- | Nothing
-- |
-- | > first (Two "hello" 42 :: Smash String Int)
-- | Just "hello"
-- | ```
first :: forall a b. Smash a b -> Maybe a
first = smash Nothing (const <<< Just)

-- | Grabs a `b` if it exists, otherwise returns nothing.
-- |
-- | ```purescript
-- | > second (Non :: Smash String Int)
-- | Nothing
-- |
-- | > second (Two "hello" 42 :: Smash String Int)
-- | Just 42
-- | ```
second :: forall a b. Smash a b -> Maybe b
second = smash Nothing (const Just)

-- | Grabs an `a` and a `b` if they exist, otherwise returns nothing.
-- |
-- | ```purescript
-- | > second (Non :: Smash String Int)
-- | Nothing
-- |
-- | > second (Two "hello" 42 :: Smash String Int)
-- | Just (Tuple "hello" 42)
-- | ```
both :: forall a b. Smash a b -> Maybe (Tuple a b)
both = smash Nothing (\a b -> Just $ Tuple a b)

-- | Checks if the constructor is `Non`.
-- |
-- | ```purescript
-- | > isNone (Non :: Smash String Int)
-- | true
-- |
-- | > isNone (Two "hello" 42 :: Smash String Int)
-- | false
-- | ```
isNone :: forall a b. Smash a b -> Boolean
isNone = smash true (const (const false))

-- | Checks if the constructor is `Two`.
-- |
-- | ```purescript
-- | > isNone (Non :: Smash String Int)
-- | false
-- |
-- | > isNone (Two "hello" 42 :: Smash String Int)
-- | true
-- | ```
isTwo :: forall a b. Smash a b -> Boolean
isTwo = smash false (const (const true))

-- | Takes all `a` and `b` values from a `Foldable` `Smash`.
-- |
-- | ```purescript
-- | > twos [Non :: Smash String Int]
-- | Nil
-- |
-- | > twos ([Two "hello" 42, Non, Two "world" 1] :: Array (Smash String Int))
-- | Cons (Tuple "hello" 42) (Cons (Tuple "world" 1) Nil)
-- | ```
twos :: forall a b f. Functor f => Foldable f => f (Smash a b) -> List (Tuple a b)
twos = catMaybes <<< foldr Cons Nil <<< map both

-- | Expand a `Smash a b` to a `Maybe a` and a `Maybe b`. Note that the tuple
-- | `Tuple (Maybe a) (Maybe b)` has more information that we can store in a
-- | `Smash a b`.
-- |
-- | ```
-- | Nothing, Nothing ~ Non
-- | Just a , Nothing ~ Non
-- | Nothing, Just b  ~ Non
-- | Just a , Just b  ~ Two a b
-- | ```
curry :: forall a b c. (Smash a b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
curry f ma mb = case ma, mb of
  Just a, Just b -> f (Two a b)
  _     , _      -> f Non

-- | Contract a `Smash a b` to a `Maybe a` and a `Maybe b`. Note that the tuple
-- | `Tuple (Maybe a) (Maybe b)` has more information that we can store in a
-- | `Smash a b`.
-- |
-- | ```
-- | Non     ~ Nothing, Nothing
-- | Two a b ~ Just a , Just b
-- | ```
uncurry :: forall a b c. (Maybe a -> Maybe b -> Maybe c) -> Smash a b -> Maybe c
uncurry f = case _ of
  Non     -> f Nothing Nothing
  Two a b -> f (Just a) (Just b)

-- | Grab all `a` and `b` values from a `Foldable` `Smash a b` and combine them
-- | each into an `Alternative` `f` tuple.
-- |
-- | ```purescript
-- | > partition [Non, Two "hello" 42, Two "world" 1] :: Tuple (List String) (List Int)
-- | Tuple (Cons "hello" (Cons "world" Nil)) (Cons 42 (Cons 1 Nil))
-- | ```
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

-- | Maps `Smash`es over a `Traversable` and partitions the values by their
-- | position in the `Smash`.
-- |
-- | ```purescript
-- | > partitionMap (\i -> if i < 3 then Non else Two i (i*10)) [1, 2, 3, 4, 5] :: Tuple (Array Int) (Array Int)
-- | Tuple [3, 4, 5] [30, 40, 50]
-- | ```
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

-- | Distribute `Smash` over a `Tuple`.
-- |
-- | ```purescript
-- | > distribute (Non :: Smash (Tuple String String) Int)
-- | Tuple Non Non
-- |
-- | > distribute (Two (Tuple "hello" "world") 42 :: Smash (Tuple String String) Int)
-- | Tuple (Two "hello" 42) (Two "world" 42)
-- | ```
distribute :: forall a b c. Smash (Tuple a b) c -> Tuple (Smash a c) (Smash b c)
distribute =
    smash
        (Tuple Non Non)
        (\(Tuple a b) c -> Tuple (Two a c) (Two b c))

-- | Distribute a `Tuple` over `Smash`.
-- |
-- | ```purescript
-- | > redistribute (Tuple Non Non :: Tuple (Smash String Int) (Smash String Int))
-- | Non
-- |
-- | > redistribute Tuple (Two "hello" 42) (Two "world" 42)
-- | Two (Tuple "hello" "world") 42
-- | ```
redistribute :: forall a b c. Tuple (Smash a c) (Smash b c) -> Smash (Tuple a b) c
redistribute = case _ of
  Tuple (Two a c) (Two b _) -> Two (Tuple a b) c
  _                         -> Non

-- | Reassociates a `Smash` from left to right.
-- | Note that `Non` and `Two Non c` both collapse to `Non`, so
-- | it is NOT the case that `reassocLR <<< reassocRL = identity`.
reassocLR :: forall a b c. Smash (Smash a b) c -> Smash a (Smash b c)
reassocLR = case _ of
  Two (Two a b) c -> Two a (Two b c)
  _               -> Non

-- | Reassociates a `Smash` from right to left.
-- | Note that `Non` and `Two Non c` both collapse to `Non`, so
-- | it is NOT the case that `reassocLR <<< reassocRL = identity`.
reassocRL :: forall a b c. Smash a (Smash b c) -> Smash (Smash a b) c
reassocRL = case _ of
  Two a (Two b c) -> Two (Two a b) c
  _               -> Non

-- | Swap the inputs for a `Smash`.
-- |
-- | ```purescript
-- | > swap (Non :: Smash String Int)
-- | Non
-- |
-- | > swap (Two "hello" 42)
-- | Two 42 "hello"
-- | ```
swap :: forall a b. Smash a b -> Smash b a
swap = smash Non (flip Two)
