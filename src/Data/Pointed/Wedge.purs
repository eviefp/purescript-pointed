-- | This module defines the `Wedge` type, along with instances and functions that
-- | help use this type. Generally, 'Wedge' is used when the data is more vague
-- | than simply using 'Either a b', i.e. when either or both 'a' and 'b' might
-- | be missing.
-- |
-- | The `Semigroup` instance may lose information (biased towards 'Eno'):
-- |
-- | > (One "hello" :: Wedge String Int) <> (Eno 42 :: Wedge String Int)
-- | Eno 42
-- |
-- | We can map over both arguments using `bimap`:
-- |
-- | > bimap show (_ + 10) (Non :: Wedge Int Int)
-- | Non
-- | > bimap show (_ + 10) (One 42 :: Wedge Int Int)
-- | One "42"
-- | > bimap show (_ + 10) (Eno 1 :: Wedge Int Int)
-- | One 11
module Data.Pointed.Wedge where

import Prelude

import Control.Alternative (class Alternative, empty, (<|>))
import Control.Biapply (class Biapply)
import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldlDefault, bifoldrDefault)
import Data.Bifunctor (class Bifunctor)
import Data.Bitraversable (class Bitraversable, bisequenceDefault, bitraverse)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldlDefault, foldr, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), catMaybes)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (class Traversable, sequenceDefault)
import Data.Tuple (Tuple(..))

-- | `Wedge` 'a' 'b' can hold either no values, an 'a', or a 'b'.  The type is
-- | isomorphic with 'Maybe (Either a b)'.
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

-- | Constructs a 'Wedge' given an isomorphic representation. Note that this is
-- | the precise inverse of 'toMaybeEither'.
-- |
-- | > fromMaybeEither Nothing :: Wedge String Int
-- | Non
-- | > fromMaybeEither (Just (Left "hello")) :: Wedge String Int
-- | One "hello"
-- | > fromMaybeEither (Just (Right 42)) :: Wedge String Int
-- | Eno 42
fromMaybeEither :: forall a b. Maybe (Either a b) -> Wedge a b
fromMaybeEither = maybe Non (either One Eno)

-- | Destructs a 'Wedge' from an isomorphic representation. Note that this is
-- | the precise inverse of 'fromMaybeEither'.
-- |
-- | > toMaybeEither (Non :: Wedge String Int)
-- | Nothing
-- | > toMaybeEither (One "hello" :: Wedge String Int)
-- | Just (Left "hello")
-- | > toMaybeEither (Eno 42 :: Wedge String Int)
-- | Just (Right 42)
toMaybeEither :: forall a b. Wedge a b -> Maybe (Either a b)
toMaybeEither = wedge Nothing (Just <<< Left) (Just <<< Right)

-- | Constructs a 'Wedge' given an either of maybes. Note that this is
-- | the precise inverse of 'toMaybeEither'.
-- |
-- | > fromEitherMaybe (Left Nothing) :: Wedge String Int
-- | Non
-- | > fromEitherMaybe (Left (Just "hello")) :: Wedge String Int
-- | One "hello"
-- | > fromEitherMaybe (Right Nothing) :: Wedge String Int
-- | Non
-- | > fromEitherMaybe (Right (Just 42)) :: Wedge String Int
-- | Eno 42
fromEitherMaybe :: forall a b. Either (Maybe a) (Maybe b) -> Wedge a b
fromEitherMaybe = either fromMaybeOne fromMaybeEno

-- | Constructs a left-biased 'Wedge'.
-- |
-- | > fromMaybeOne Nothing :: Wedge String Int
-- | Non
-- | > fromMaybeOne (Just "hello") :: Wedge String Int
-- | One "hello"
fromMaybeOne :: forall a b. Maybe a -> Wedge a b
fromMaybeOne = maybe Non One

-- | Constructs a right-biased 'Wedge'.
-- |
-- | > fromMaybeTwo Nothing :: Wedge String Int
-- | Non
-- | > fromMaybeTwo (Just 42) :: Wedge String Int
-- | Eno 42
fromMaybeEno :: forall a b. Maybe b -> Wedge a b
fromMaybeEno = maybe Non Eno

-- | 'Wedge' catamorphism (fold). Takes an input for each possible constructor and
-- | translates it to 'c'.
-- |
-- | For example, we can go from some 'w :: Wedge a b' to a
-- | 'Maybe (Either a b)' using:
-- |
-- | > wedge Nothing Left Right (Non :: Wedge String Int)
-- | Non
-- | > wedge Nothing Left Right (One "hello" :: Wedge String Int)
-- | Just (Left "hello")
-- | > wedge Nothing Left Right (Eno 42 :: Wedge String Int)
-- | Just (Right 42)
wedge :: forall a b c. c -> (a -> c) -> (b -> c) -> Wedge a b -> c
wedge non one eno = case _ of
  Non   -> non
  One a -> one a
  Eno b -> eno b

-- | Grabs an 'a' if it exists, otherwise returns nothing.
-- |
-- | > first (Non :: Wedge String Int)
-- | Nothing
-- | > first (One "hello" :: Wedge String Int)
-- | Just "hello"
-- | > first (Eno 42 :: Wedge String Int)
-- | Nothing
first :: forall a b. Wedge a b -> Maybe a
first = wedge Nothing Just (const Nothing)

-- | Grabs a 'b' if it exists, otherwise returns nothing.
-- |
-- | > second (Non :: Wedge String Int)
-- | Nothing
-- | > second (One "hello" :: Wedge String Int)
-- | Nothing
-- | > second (Eno 42 :: Wedge String Int)
-- | Just 42
second :: forall a b. Wedge a b -> Maybe b
second = wedge Nothing (const Nothing) Just

-- | Checks if the constructor is `Non`.
-- |
-- | > isNone (Non :: Wedge String Int)
-- | true
-- | > isNone (One "hello" :: Wedge String Int)
-- | false
-- | > isNone (Eno 42 :: Wedge String Int)
-- | false
isNone :: forall a b. Wedge a b -> Boolean
isNone = wedge true (const false) (const false)

-- | Checks if the constructor is `One`.
-- |
-- | > isOne (Non :: Wedge String Int)
-- | false
-- | > isOne (One "hello" :: Wedge String Int)
-- | true
-- | > isOne (Eno 42 :: Wedge String Int)
-- | false
isOne :: forall a b. Wedge a b -> Boolean
isOne = wedge false (const true) (const false)

-- | Checks if the constructor is `Eno`.
-- |
-- | > isEno (Non :: Wedge String Int)
-- | false
-- | > isEno (One "hello" :: Wedge String Int)
-- | false
-- | > isEno (Eno 42 :: Wedge String Int)
-- | true
isEno :: forall a b. Wedge a b -> Boolean
isEno = wedge false (const false) (const true)

-- | Takes all 'a' values from a `Foldable` `Wedge`.
-- |
-- | > ones [Non :: Wedge String Int]
-- | Nil
-- | > ones ([One "hello", One "world"] :: Array (Wedge String Int))
-- | Cons "hello" (Cons "world" Nil)
-- | > ones ([Eno 42, Eno 13] :: Array (Wedge String Int))
-- | Nil
-- | > ones ([One "hello", Eno 42] :: Array (Wedge String Int))
-- | Cons "hello" Nil
ones :: forall a b f. Functor f => Foldable f => f (Wedge a b) -> List a
ones = catMaybes <<< foldr Cons Nil <<< map first

-- | Takes all 'b' values from a `Foldable` `Wedge`.
-- |
-- | > enos [Non :: Wedge String Int]
-- | Nil
-- | > enos ([One "hello", One "world"] :: Array (Wedge String Int))
-- | Nil
-- | > enos ([Eno 42, Eno 13] :: Array (Wedge String Int))
-- | Cons 42 (Cons 13 Nil)
-- | > enos ([One "hello", Eno 42] :: Wedge String Int)
-- | Cons 42 Nil
enos :: forall a b f. Functor f => Foldable f => f (Wedge a b) -> List b
enos = catMaybes <<< foldr Cons Nil <<< map second

-- | Grab all 'a' and 'b' values from a `Foldable` 'Wedge a b' and combine them
-- | each into an `Alternative` 'f' tuple.
-- |
-- | > partition [Non, One "hello", Eno 42] :: Tuple (List String) (List Int)
-- | Tuple (Cons "hello" Nil) (Cons 42)
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

-- | Maps 'Wedge's over a `Traversable` and partitions the values by their
-- | position in the 'Wedge'.
-- |
-- | > partitionMap (\i -> if i < 3 then One i else Eno i) [1, 2, 3, 4, 5] :: Tuple (Array Int) (Array Int)
-- | Tuple [1, 2] [3, 4, 5]
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

-- | Distribute 'Wedge' over a 'Tuple'.
-- |
-- | > distribute (Non :: Wedge (Tuple String String) Int)
-- | Tuple Non Non
-- | > distribute (One (Tuple "hello" "world") :: Wedge (Tuple String String) Int)
-- | Tuple (One "hello") (One "world")
-- | > distribute (Eno 42 :: Wedge (Tuple String String) Int)
-- | Tuple (Eno 42) (Eno 42)
distribute :: forall a b c. Wedge (Tuple a b) c -> Tuple (Wedge a c) (Wedge b c)
distribute = case _ of
  Non             -> Tuple Non Non
  One (Tuple a b) -> Tuple (One a) (One b)
  Eno c           -> Tuple (Eno c) (Eno c)

-- | Codistribute 'Either' over a 'Wedge'.
-- |
-- | > codistribute (Left Non :: Either (Wedge String Int) (Wedge Boolean Int))
-- | Non
-- | > codistribute (Left (One "hello") :: Either (Wedge String Int) (Wedge Boolean Int))
-- | One (Left "hello")
-- | > codistribute (Left (Eno 42) :: Either (Wedge String Int) (Wedge Boolean Int))
-- | Eno 42
-- | > codistribute (Right Non :: Either (Wedge String Int) (Wedge Boolean Int))
-- | Non
-- | > codistribute (Right (One true) :: Either (Wedge String Int) (Wedge Boolean Int))
-- | One (Right true)
-- | > codistribute (Right (Eno 42) :: Either (Wedge String Int) (Wedge Boolean Int))
-- | Eno 42
codistribute :: forall a b c. Either (Wedge a c) (Wedge b c) -> Wedge (Either a b) c
codistribute =
  wedge Non (One <<< Left) Eno
  `either`
  wedge Non (One <<< Right) Eno

-- | Reassociates a `Wedge` from left to right.
-- | Note that this is the inverse of 'reassocRL', i.e.:
-- | 'reassocRL <<< reassocLR = identity'.
reassocLR :: forall a b c. Wedge (Wedge a b) c -> Wedge a (Wedge b c)
reassocLR = case _ of
  Non         -> Non
  One Non     -> Eno Non
  One (One a) -> One a
  One (Eno b) -> Eno (One b)
  Eno c       -> Eno (Eno c)

-- | Reassociates a `Wedge` from right to left.
-- | Note that this is the inverse of 'reassocLR', i.e.:
-- | 'reassocRL <<< reassocLR = identity'.
reassocRL :: forall a b c. Wedge a (Wedge b c) -> Wedge (Wedge a b) c
reassocRL = case _ of
  Non         -> Non
  One a       -> One (One a)
  Eno Non     -> One Non
  Eno (One b) -> One (Eno b)
  Eno (Eno c) -> Eno c

-- | Swap the inputs for a 'Wedge'.
-- |
-- | > swap (Non :: Wedge String Int)
-- | Non
-- | > swap (One "hello" :: Wedge String Int)
-- | Eno "hello"
-- | > swap (Eno 42 :: Wedge String Int)
-- | One 42
swap :: forall a b. Wedge a b -> Wedge b a
swap = wedge Non Eno One
