module Data.Profunctor.Optics.Affine where

import Prelude

import Data.Bifunctor (lmap)
import Data.Bifunctor as B
import Data.Either (Either(..), either)
import Data.Lens (Lens, _Right, set, view)
import Data.Profunctor (class Profunctor, lcmap, rmap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong, first)
import Data.Tuple (Tuple(..), uncurry)

type Affine s t a b = forall p. Strong p => Choice p => p a b -> p s t
type Affine' s a = Affine s s a a

data Stall a b s t = Stall (s -> b -> t) (s -> Either t a)

instance functorStall :: Functor (Stall a b s) where
  map f (Stall u p) =
    Stall (map f <<< u) (lmap f <<< p)

instance profunctorStall :: Profunctor (Stall a b) where
  dimap f g (Stall u p) =
    Stall (map g <<< u <<< f) (lmap g <<< p <<< f)

instance strongStall :: Strong (Stall a b) where
  first (Stall u p) =
    Stall (\(Tuple s x) b -> Tuple (u s b) x)
          (\(Tuple s x) -> lmap (\t -> Tuple t x) (p s))

  second (Stall u p) =
    Stall (\(Tuple x s) b -> Tuple x (u s b))
          (\(Tuple x s) -> lmap (Tuple x) (p s))

instance choiceStall :: Choice (Stall a b) where
  left (Stall u p) =
    Stall
      (case _ of
        Left s -> \b -> Left (u s b)
        Right x -> \_ -> Right x)
      (case _ of
        Left s -> lmap Left (p s)
        Right x -> Left (Right x))

  right (Stall u p) =
    Stall
      (case _ of
        Left x -> \_ -> Left x
        Right s -> \b -> Right (u s b))
      (case _ of
        Left x -> Left (Left x)
        Right s -> lmap Right (p s))

tryView :: forall s t a b. Stall a b s t -> s -> Either t a
tryView (Stall _ x) = x

replaceIfPresent :: forall s t a b. Stall a b s t -> s -> b -> t
replaceIfPresent (Stall x _) = x

ac2ap :: forall s t a b. Stall a b s t -> Affine s t a b
ac2ap l =
      -- p a b                                -> p (Either x a) (Either x b)
      _Right
      -- p (Either t a) (Either t b)          -> p s (Either t b)
  >>> lcmap (tryView l)
      -- p s (Either t b)                     -> p (Tuple s x) (Tuple (Either t b) x)
  >>> first
      -- p (Tuple s s) (Tuple (Either t b) s) -> p s (Tuple (Either t b) s)
  >>> lcmap dup
      -- p s (Tuple (Either t b) s)           -> p s t
  >>> rmap (uncurry $ \e s -> either identity ((replaceIfPresent l) s) e)
  where
  dup :: forall x. x -> Tuple x x
  dup x = Tuple x x

ap2ac :: forall s t a b. Affine s t a b -> Stall a b s t
ap2ac l = l (Stall (const identity) Right)

focusAffine :: forall f s t a b. (forall x y. Lens (f x) (f y) x y) -> Affine s t a b -> Affine (f s) t (f a) b
focusAffine l a' = ac2ap (Stall put get)
  where
  a = ap2ac a'
  get fs = B.rmap (flip (set l) fs) <<< tryView a <<< view l $ fs
  put fs b = replaceIfPresent a (view l fs) b
