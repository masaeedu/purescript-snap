module Data.Profunctor.Optics.Spinal where

import Prelude

import Control.Biapply as B
import Data.Bifunctor as B
import Data.Either (Either, either)
import Data.Lens as L
import Data.Tuple (Tuple(..))
import Data.Tensor (transpose)

infixl 7 type Either as +
infixl 7 type Tuple  as *
infixl 7      Tuple  as *

leverywhere :: forall s t a b u v c d.
  L.Lens s t a b ->
  L.Lens u v c d ->
  L.Lens (s * u) (t * v) (a * c) (b * d)
  -- can't do Lens s t (a * c) (b * d) without demanding independence of foci
leverywhere l1 l2 = L.lens view (flip set)
  where
  view = B.bimap   (L.view l1) (L.view l2)
  set  = B.bilift2 (L.set  l1) (L.set  l2)

lanywhere :: forall s t a b u v.
  L.Lens s t a b ->
  L.Lens u v a b ->
  L.Lens (s + u) (t + v) a b
  -- can't do Lens (s + u) (t + v) (a + c) (b + d) without demanding coherence of foci
lanywhere l1 l2 = L.lens view (flip set)
  where
  view  = either  (L.view l1  ) (L.view l2  )
  set b = B.bimap (L.set  l1 b) (L.set  l2 b)

panywhere :: forall s t a b u v c d.
  L.Prism s t a b ->
  L.Prism u v c d ->
  L.Prism (s + u) (t + v) (a + c) (b + d)
  -- can't do Prism s t (a + c) (b + d) without demanding independence of foci
panywhere l1 l2 = L.prism review matching
  where
  review   = B.bimap (L.review   l1) (L.review   l2)
  matching = B.bimap (L.matching l1) (L.matching l2) >>> transpose

-- psomewhere :: forall s t a b u v c d.
--   L.Prism s t a b ->
--   L.Prism u v a b ->
--   L.Prism (s * u) (t * v) a b
-- psomewhere l1 l2 = L.prism review matching
--   where
--   review   = ?1
--   matching = ?2

-- (s + u) -> (t + v) + (a + c)

{-
Can we get:

- Given a product of lenses from some whole     onto various parts, a lens  from the whole           onto a product of the various parts
- Given a product of lenses from various wholes onto some part    , a lens  from a sum of wholes     onto the part
- Given a product of prisms from some whole     onto various parts, a prism from the whole           onto a sum of the various parts
- Given a product of prisms from various wholes onto some part    , a prism from a product of wholes onto the part


everywhere :: (Lens s t a b * Lens u v c d) -> Lens (s * u) (t * v) (a * c) (b * d) -- can't do Lens s       t       (a * c) (b * d) without demanding independence of foci
anywhere   :: (Lens s t a b * Lens u v a b) -> Lens (s + u) (t + v) a       b       -- can't do Lens (s + u) (t + v) (a + c) (b + d) without demanding coherence    of foci

everywhere :: (Prsm s t a b * Prsm u v c d) -> Prsm (s * t) (u * v) (a * c) (b * d) -- can't do Prsm s t (a * c) (b * d) without
anywhere   :: (Prsm s t a b * Prsm u v c d) -> Prsm (s + u) (t + v) (a + c) (b + d)
-}
