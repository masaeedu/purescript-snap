module Control.Monad.Supply.Trans where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, class Plus)
import Control.Monad.Cont (class MonadCont)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT(..), ask, local)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState)
import Control.Monad.Supply.Class (class MonadSupply)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (class MonadTell, class MonadWriter)
import Control.MonadPlus (class MonadPlus, class MonadZero)
import Data.Newtype (class Newtype)
import Effect.Class (class MonadEffect)

newtype SupplyT s m a = SupplyT (ReaderT (m s) m a)

derive instance newtypeSupplyT :: Newtype (SupplyT s m a) _

derive newtype instance functorSupplyT     :: Functor m => Functor (SupplyT s m)
derive newtype instance applySupplyT       :: Apply m => Apply (SupplyT s m)
derive newtype instance applicativeSupplyT :: Applicative m => Applicative (SupplyT s m)
derive newtype instance altSupplyT         :: Alt m => Alt (SupplyT s m)
derive newtype instance plusSupplyT        :: Plus m => Plus (SupplyT s m)
derive newtype instance alternativeSupplyT :: Alternative m => Alternative (SupplyT s m)
derive newtype instance bindSupplyT        :: Bind m => Bind (SupplyT s m)
derive newtype instance monadSupplyT       :: Monad m => Monad (SupplyT s m)
derive newtype instance monadZeroSupplyT   :: MonadZero m => MonadZero (SupplyT s m)
derive newtype instance semigroupSupplyT   :: (Apply m, Semigroup a) => Semigroup (SupplyT s m a)
derive newtype instance monoidSupplyT      :: (Applicative m, Monoid a) => Monoid (SupplyT s m a)
derive newtype instance monadPlusSupplyT   :: MonadPlus m => MonadPlus (SupplyT s m)
derive newtype instance monadTransSupplyT  :: MonadTrans (SupplyT s)
derive newtype instance monadEffectSupplyT :: MonadEffect m => MonadEffect (SupplyT s m)
derive newtype instance monadContSupplyT   :: MonadCont m => MonadCont (SupplyT s m)
derive newtype instance monadThrowSupplyT  :: MonadThrow e m => MonadThrow e (SupplyT s m)
derive newtype instance monadErrorSupplyT  :: MonadError e m => MonadError e (SupplyT s m)
derive newtype instance monadStateSupplyT  :: MonadState s' m => MonadState s' (SupplyT s m)
derive newtype instance monadTellSupplyT   :: MonadTell w m => MonadTell w (SupplyT s m)
derive newtype instance monadWriterSupplyT :: MonadWriter w m => MonadWriter w (SupplyT s m)
derive newtype instance monadRecSupplyT    :: MonadRec m => MonadRec (SupplyT s m)

instance monadAskSupplyT :: MonadAsk r m => MonadAsk r (SupplyT s m)
  where
  ask = lift $ ask

instance monadReaderSupplyT :: MonadReader r m => MonadReader r (SupplyT s m)
  where
  local f (SupplyT (ReaderT x)) = SupplyT $ ReaderT $ local f <<< x

instance monadSupplySupplyT :: Monad m => MonadSupply s (SupplyT s m)
  where
  supply = SupplyT $ ReaderT $ identity
