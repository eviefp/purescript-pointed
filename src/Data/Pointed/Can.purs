-- | This module defines the type `Can`, along with instances and functions that
-- | help use this type. Generally, `Can` is used when the data is more vague
-- | than simply using `Either a b` or `Tuple a b`, i.e. when either `a` or `b`
-- | might be missing or be present independently.
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

-- | `Can a b` can hold either no values, an `a`, a `b`, or both an `a` and
-- | a `b`. The type is isomorphic with `Maybe (Either (Either a b) (Tuple a b))`.
data Can a b
  = Non
  | One a
  | Eno b
  | Two a b

-- | `Can` values can be compared. Note that they are only equal if they are
-- | precisely equal.
-- |
-- | ```purescript
-- | > (One "hello" :: Can String Int) == (Two "hello" 42 :: Can String Int)
-- | false
-- | ```
derive instance eqCan :: (Eq a, Eq b) => Eq (Can a b)

derive instance ordCan :: (Ord a, Ord b) => Ord (Can a b)

derive instance genericCan :: Generic (Can a b) _

derive instance functorCan :: Functor (Can a)

-- | The `Semigroup` instance will never lose information and requires both
-- | `a` and `b` to have `Semigroup` instances:
-- |
-- | ```purescript
-- | > (One "hello" :: Can String String) <> Two "world" "42"
-- | Two "helloworld" "42"
-- | ```
instance semigroupCan :: (Semigroup a, Semigroup b) => Semigroup (Can a b) where
  append c1 c2 = case c1, c2 of
    Non    , b       -> b
    b      , Non     -> b
    One a  , One b   -> One (a <> b)
    One a  , Eno b   -> Two a b
    One a  , Two b c -> Two (a <> b) c
    Eno a  , Eno b   -> Eno (a <> b)
    Eno b  , One a   -> Two a b
    Eno b  , Two a c -> Two a (b <> c)
    Two a b, Two c d -> Two (a <> c) (b <> d)
    Two a b, One c   -> Two (a <> c) b
    Two a b, Eno c   -> Two a (b <> c)

instance monoidCan :: (Semigroup a, Semigroup b) => Monoid (Can a b) where
  mempty = Non

-- | ```purescript
-- | > bimap show (_ + 10) (Two 42 32)
-- | Two "42" 42
-- | ```
instance bifunctorCan :: Bifunctor Can where
  bimap f g = case _ of
    Non     -> Non
    One a   -> One (f a)
    Eno b   -> Eno (g b)
    Two a b -> Two (f a) (g b)

instance biApplyCan :: Biapply Can where
  biapply c1 c2 = case c1, c2 of
    One f  , One a -> One (f a)
    Eno g  , Eno b -> Eno (g b)
    Two f g, rest  -> bimap f g rest
    _      , _       -> Non

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
  apply c1 c2 = case c1, c2 of
    _      , Non     -> Non
    Non    , _       -> Non
    One a  , _       -> One a
    Eno _  , One b   -> One b
    Eno f  , Eno a   -> Eno (f a)
    Eno f  , Two a b -> Two a (f b)
    Two a _, One b   -> One (a <> b)
    Two a f, Eno b   -> Two a (f b)
    Two a f, Two b c -> Two (a <> b) (f c)

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

-- | Constructs a `Can` given an isomorphic representation. Note that this is
-- | the precise inverse of `toRepr`.
-- |
-- | ```purescript
-- | > fromRepr Nothing :: Can String Int
-- | Non
-- |
-- | > fromRepr (Just (Left (Left "hello"))) :: Can String Int
-- | One "hello"
-- |
-- | > fromRepr (Just (Left (Right 42))) :: Can String Int
-- | Eno 42
-- |
-- | > fromRepr (Just (Right (Tuple "hello" 42))) :: Can String Int
-- | Two "hello" 42
-- | ```
fromRepr :: forall a b. Maybe (Either (Either a b) (Tuple a b)) -> Can a b
fromRepr = case _ of
  Nothing                  -> Non
  Just (Left (Left a))     -> One a
  Just (Left (Right b))    -> Eno b
  Just (Right (Tuple a b)) -> Two a b

-- | Destructs a `Can` to its isomorphic representation. Note that this is
-- | the precise inverse of `fromRepr`.
-- |
-- | ```purescript
-- | > toRepr (Non :: Can String Int)
-- | Nothing
-- |
-- | > toRepr (One "hello" :: Can String Int)
-- | Just (Left (Left "hello"))
-- |
-- | > toRepr (Eno 42 :: Can String Int)
-- | Just (Left (Right 42))
-- |
-- | > toRepr Two "hello" 42
-- | Just (Right (Tuple "hello" 42))
-- | ```
toRepr :: forall a b. Can a b -> Maybe (Either (Either a b) (Tuple a b))
toRepr = case _ of
  Non     -> Nothing
  One a   -> Just (Left (Left a))
  Eno b   -> Just (Left (Right b))
  Two a b -> Just (Right (Tuple a b))

-- | Construct a `Can` from a pair of `Maybe`s.
-- |
-- | ```purescript
-- | > fromMaybe Nothing Nothing :: Can String Int
-- | Non
-- |
-- | > fromMaybe (Just "hello") Nothing :: Can String Int
-- | One "hello"
-- |
-- | > fromMaybe Nothing (Just 42) :: Can String Int
-- | Eno 42
-- |
-- | > fromMaybe (Just "hello") (Just 42) :: Can String Int
-- | Two "hello" 42
-- | ```
fromMaybe :: forall a b. Maybe a -> Maybe b -> Can a b
fromMaybe = case _, _ of
  Nothing , Nothing -> Non
  Just a  , Nothing -> One a
  Nothing , Just b  -> Eno b
  Just a  , Just b  -> Two a b

-- | `Can` catamorphism (fold). Takes an input for each possible constructor and
-- | translates it to `c`.
-- |
-- | For example, we can go from some `c :: Can a b` to a
-- | `Maybe (Either (Either a b) (Tuple a b))` using:
-- |
-- | ```purescript
-- | > can Nothing (Left <<< Left) (Left <<< Right) (\a b -> Right (Tuple a b)) (Non :: Can String Int)
-- | Nothing
-- |
-- | > can Nothing (Left <<< Left) (Left <<< Right) (\a b -> Right (Tuple a b)) (One "hello" :: Can String Int)
-- | Just (Left (Left "hello"))
-- |
-- | > can Nothing (Left <<< Left) (Left <<< Right) (\a b -> Right (Tuple a b)) (Eno 42 :: Can String Int)
-- | Just (Left (Right 42))
-- |
-- | > can Nothing (Left <<< Left) (Left <<< Right) (\a b -> Right (Tuple a b)) (Two "hello" 42 :: Can String Int)
-- | Just (Right (Tuple "hello" 42))
-- | ```
can :: forall a b c. c -> (a -> c) -> (b -> c) -> (a -> b -> c) -> Can a b -> c
can non one eno two = case _ of
  Non     -> non
  One a   -> one a
  Eno b   -> eno b
  Two a b -> two a b

-- | Grabs an `a` if it exists, otherwise returns nothing. Note that this
-- | function grabs an `a` from either the `One` or the `Two` constructors.
-- |
-- | ```purescript
-- | > first (Non :: Can String Int)
-- | Nothing
-- |
-- | > first (One "hello" :: Can String Int)
-- | Just "hello"
-- |
-- | > first (Eno 42 :: Can String Int)
-- | Nothing
-- |
-- | > first (Two "hello" 42)
-- | Just "hello"
-- | ```
first :: forall a b. Can a b -> Maybe a
first = can Nothing Just (const Nothing) (const <<< Just)

-- | Grabs a `b` if it exists, otherwise returns nothing. Note that this
-- | function grabs a `b` from either the `Eno` or the `Two` constructors.
-- |
-- | ```purescript
-- | > second (Non :: Can String Int)
-- | Nothing
-- |
-- | > second (One "hello" :: Can String Int)
-- | Nothing
-- |
-- | > second (Eno 42 :: Can String Int)
-- | Just 42
-- |
-- | > second (Two "hello" 42)
-- | Just 42
-- | ```
second :: forall a b. Can a b -> Maybe b
second = can Nothing (const Nothing) Just (const Just)

-- | Grabs both an `a` and a `b` if they both exist, from the `Two` constructor.
-- |
-- | ```purescript
-- | > both (Non :: Can String Int)
-- | Nothing
-- |
-- | > both (One "hello" :: Can String Int)
-- | Nothing
-- |
-- | > both (Eno 42 :: Can String Int)
-- | Nothing
-- |
-- | > both (Two "hello" 42)
-- | Just (Tuple "hello" 42)
-- | ```
both :: forall a b. Can a b -> Maybe (Tuple a b)
both = can Nothing (const Nothing) (const Nothing) (\a -> Just <<< Tuple a)

-- | Checks if the constructor is `Non`.
-- |
-- | ```purescript
-- | > isNone (Non :: Can String Int)
-- | true
-- |
-- | > isNone (One "hello" :: Can String Int)
-- | false
-- |
-- | > isNone (Eno 42 :: Can String Int)
-- | false
-- |
-- | > isNone (Two "hello" 42)
-- | false
-- | ```
isNone :: forall a b. Can a b -> Boolean
isNone = can true (const false) (const false) (const $ const false)

-- | Checks if the constructor is `One`.
-- |
-- | ```purescript
-- | > isOne (Non :: Can String Int)
-- | false
-- |
-- | > isOne (One "hello" :: Can String Int)
-- | true
-- |
-- | > isOne (Eno 42 :: Can String Int)
-- | false
-- |
-- | > isOne (Two "hello" 42)
-- | false
-- | ```
isOne :: forall a b. Can a b -> Boolean
isOne = can false (const true) (const false) (const $ const false)

-- | Checks if the constructor is `Eno`.
-- |
-- | ```purescript
-- | > isEno (Non :: Can String Int)
-- | false
-- |
-- | > isEno (One "hello" :: Can String Int)
-- | false
-- |
-- | > isEno (Eno 42 :: Can String Int)
-- | true
-- |
-- | > isEno (Two "hello" 42)
-- | false
-- | ```
isEno :: forall a b. Can a b -> Boolean
isEno = can false (const false) (const true) (const $ const false)

-- | Checks if the constructor is `Two`.
-- |
-- | ```purescript
-- | > isTwo (Non :: Can String Int)
-- | false
-- |
-- | > isTwo (One "hello" :: Can String Int)
-- | false
-- |
-- | > isTwo (Eno 42 :: Can String Int)
-- | false
-- |
-- | > isTwo (Two "hello" 42)
-- | true
-- | ```
isTwo :: forall a b. Can a b -> Boolean
isTwo = can false (const false) (const false) (const $ const true)

-- | Takes all `a` values from a `Foldable` `Can`. Note that values are extracted
-- | from both the `One` and `Two` constructors.
-- |
-- | ```purescript
-- | > ones [Non :: Can String Int]
-- | Nil
-- |
-- | > ones ([One "hello", One "world"] :: Array (Can String Int))
-- | Cons "hello" (Cons "world" Nil)
-- |
-- | > ones ([Eno 42, Eno 13] :: Array (Can String Int))
-- | Nil
-- |
-- | > ones ([Two "hello" 42, Two "world" 1] :: Array (Can String Int))
-- | Cons "hello" (Cons "world" Nil)
-- |
-- | > ones [One "hello", Eno 42, Two "world" 13]
-- | Cons "hello" (Cons "world" Nil)
-- | ```
ones :: forall a b f. Functor f => Foldable f => f (Can a b) -> List a
ones = catMaybes <<< foldr Cons Nil <<< map first

-- | Takes all `b` values from a `Foldable` `Can`. Note that values are extracted
-- | from both the `Eno` and `Two` constructors.
-- |
-- | ```purescript
-- | > enos [Non :: Can String Int]
-- | Nil
-- |
-- | > enos ([One "hello", One "world"] :: Array (Can String Int))
-- | Nil
-- |
-- | > enos ([Eno 42, Eno 13] :: Array (Can String Int))
-- | Cons 42 (Cons 13 Nil)
-- |
-- | > enos ([Two "hello" 42, Two "world" 13] :: Array (Can String Int))
-- | Cons 42 (Cons 13 Nil)
-- |
-- | > enos [One "hello", Eno 42, Two "world" 13]
-- | Cons 42 (Cons 13 Nil)
-- | ```
enos :: forall a b f. Functor f => Foldable f => f (Can a b) -> List b
enos = catMaybes <<< foldr Cons Nil <<< map second

-- | Takes all `a` and `b` values from a `Foldable` `Can`. Note that values are
-- | only extracted from the `Two` constructor.
-- |
-- | ```purescript
-- | > twos [Non :: Can String Int]
-- | Nil
-- |
-- | > twos ([One "hello", One "world"] :: Array (Can String Int))
-- | Nil
-- |
-- | > twos ([Eno "hello", Eno "world"] :: Array (Can String Int))
-- | Nil
-- |
-- | > twos ([Two "hello" 42, Two "world" 1] :: Array (Can String Int))
-- | Cons (Tuple "hello" 42) (Cons (Tuple "world" 1) Nil)
-- | ```
twos :: forall a b f. Functor f => Foldable f => f (Can a b) -> List (Tuple a b)
twos = catMaybes <<< foldr Cons Nil <<< map both

-- | Expand a `Can a b` to a `Maybe a` and a `Maybe b`. Note that the tuple
-- | `Tuple (Maybe a) (Maybe b)` is exactly equivalent with `Can a b`:
-- |
-- | ```
-- | - Nothing, Nothing ~ Non
-- | - Just a , Nothing ~ One a
-- | - Nothing, Just b  ~ Eno b
-- | - Just a , Just b  ~ Two a b
-- | ```
curry :: forall a b c. (Can a b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
curry f ma mb = case ma, mb of
  Nothing, Nothing -> f Non
  Just a , Nothing -> f (One a)
  Nothing, Just b  -> f (Eno b)
  Just a , Just b  -> f (Two a b)

-- | Contract a `Maybe a` and a `Maybe b` to a `Can a b`. Note that `Can a b`
-- | is exactly equivalent with `Tuple (Maybe a) (Maybe b)`:
-- |
-- | ```
-- | Non     ~ Nothing, Nothing
-- | One a   ~ Just a , Nothing
-- | Eno b   ~ Nothing, Just b
-- | Two a b ~ Just a , Just b
-- | ```
uncurry :: forall a b c. (Maybe a -> Maybe b -> Maybe c) -> Can a b -> Maybe c
uncurry f = case _ of
  Non     -> f Nothing Nothing
  One a   -> f (Just a) Nothing
  Eno b   -> f Nothing (Just b)
  Two a b -> f (Just a) (Just b)

-- | Grab all `a` and `b` values from a `Foldable` `Can a b` and combine them
-- | each into an `Alternative` `f` tuple.
-- |
-- | ```purescript
-- | > partition [Non, One "hello", Eno 42, Two "world" 1] :: Tuple (List String) (List Int)
-- | Tuple (Cons "hello" (Cons "world" Nil)) (Cons 42 (Cons 1 Nil))
-- | ```
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

-- | Maps `Can`s over a `Traversable` and partitions the values by their
-- | position in the `Can`.
-- |
-- | ```purescript
-- | > partitionMap (\i -> if i < 3 then One i else Eno i) [1, 2, 3, 4, 5] :: Tuple (Array Int) (Array Int)
-- | Tuple [1, 2] [3, 4, 5]
-- | ```
partitionMap
  :: forall f t a b c
   . Alternative f
  => Traversable t
  => (a -> Can b c)
  -> t a
  -> Tuple (f b) (f c)
partitionMap f = foldr go (Tuple empty empty)
  where
    go :: a -> Tuple (f b) (f c) -> Tuple (f b) (f c)
    go a original@(Tuple fb fc) = case f a of
      Non     -> original
      One b   -> Tuple (fb <|> pure b) fc
      Eno c   -> Tuple fb (fc <|> pure c)
      Two b c -> Tuple (fb <|> pure b) (fc <|> pure c)

-- | Distribute `Can` over a `Tuple`.
-- |
-- | ```purescript
-- | > distribute (Non :: Can (Tuple String String) Int)
-- | Tuple Non Non
-- |
-- | > distribute (One (Tuple "hello" "world") :: Can (Tuple String String) Int)
-- | Tuple (One "hello") (One "world")
-- |
-- | > distribute (Eno 42 :: Can (Tuple String String) Int)
-- | Tuple (Eno 42) (Eno 42)
-- |
-- | > distribute (Two (Tuple "hello" "world") 42 :: Can (Tuple String String) Int)
-- | Tuple (Two "hello" 42) (Two "world" 42)
-- | ```
distribute :: forall a b c. Can (Tuple a b) c -> Tuple (Can a c) (Can b c)
distribute = case _ of
  Non               -> Tuple Non Non
  One (Tuple a b)   -> Tuple (One a) (One b)
  Eno c             -> Tuple (Eno c) (Eno c)
  Two (Tuple a b) c -> Tuple (Two a c) (Two b c)

-- | Codistribute `Either` over a `Can`.
-- |
-- | ```purescript
-- | > codistribute (Left Non :: Either (Can String Int) (Can Boolean Int))
-- | Non
-- |
-- | > codistribute (Left (One "hello") :: Either (Can String Int) (Can Boolean Int))
-- | One (Left "hello")
-- |
-- | > codistribute (Left (Eno 42) :: Either (Can String Int) (Can Boolean Int))
-- | Eno 42
-- |
-- | > codistribute (Left (Two "hello" 42) :: Either (Can String Int) (Can Boolean Int))
-- | Two (Left "hello") 42
-- |
-- | > codistribute (Right Non :: Either (Can String Int) (Can Boolean Int))
-- | Non
-- |
-- | > codistribute (Right (One true) :: Either (Can String Int) (Can Boolean Int))
-- | One (Right true)
-- |
-- | > codistribute (Right (Eno 42) :: Either (Can String Int) (Can Boolean Int))
-- | Eno 42
-- |
-- | > codistribute (Right (Two true 42) :: Either (Can String Int) (Can Boolean Int))
-- | Two (Right true) 42
-- | ```
codistribute :: forall a b c. Either (Can a c) (Can b c) -> Can (Either a b) c
codistribute =
    can Non (One <<< Left ) Eno (\a c -> Two (Left a ) c)
    `either`
    can Non (One <<< Right) Eno (\b c -> Two (Right b) c)

-- | Reassociates a `Can` from left to right.
-- | Note that `Eno c` and `Two Non c` both collapse to `Eno (Eno c)`, so
-- | it is NOT the case that `reassocLR <<< reassocRL = identity`.
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

-- | Reassociates a `Can` from right to left.
-- | Note that `Eno c` and `Two Non c` both collapse to `Eno (Eno c)`, so
-- | it is NOT the case that `reassocLR <<< reassocRL = identity`.
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

-- | Swap the inputs for a `Can`.
-- |
-- | ```purescript
-- | > swap (Non :: Can String Int)
-- | Non
-- |
-- | > swap (One "hello" :: Can String Int)
-- | Eno "hello"
-- |
-- | > swap (Eno 42 :: Can String Int)
-- | One 42
-- |
-- | > swap (Two "hello" 42)
-- | Two 42 "hello"
-- | ```
swap :: forall a b. Can a b -> Can b a
swap = can Non Eno One (flip Two)
