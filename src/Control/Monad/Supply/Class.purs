module Control.Monad.Supply.Class where

import Control.Monad (class Monad)

class Monad m <= MonadSupply s m | m -> s
  where
  supply :: m s
