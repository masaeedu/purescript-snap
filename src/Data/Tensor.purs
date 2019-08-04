module Data.Tensor where

import Prelude hiding ((*))

import Data.Bifunctor as B
import Data.Either (Either(..), either)
import Data.Lens (Iso')
import Data.Tuple (Tuple(..))

infixl 7 type Either as +
infixl 7 type Tuple  as *
infixl 7      Tuple  as *

class Semigroupal p where
  assoc :: forall a b c. Iso' (p (p a b) c) (p a (p b c))

class Semigroupal p <= Monoidal p i where
  left  :: forall a. Iso' (p a i) a
  right :: forall a. Iso' (p i a) a

class Semigroupal p <= Braided p where
  braiding :: forall a b. Iso' (p a b) (p b a)

class Duoidal p q where
  transpose :: forall a b c d. q (p a b) (p c d) -> p (q a c) (q b d)

instance duoidalEitherEither :: Duoidal Either Either where
  transpose = either (either (Left <<< Left) (Right <<< Left)) (either (Left <<< Right) (Right <<< Right))

instance duoidalTupleTuple :: Duoidal Tuple Tuple where
  transpose ((a * b) * (c * d)) = (a * c) * (b * d)

instance duoidalTupleEither :: Duoidal Tuple Either where
  transpose = either (B.bimap Left Left) (B.bimap Right Right)
