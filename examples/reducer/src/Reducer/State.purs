module Examples.Reducer.State where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug.Trace (trace)
import Effect.Aff (Milliseconds(..), delay, Aff)

data DelayerAction
  = Loading
  | Loaded String

instance showDelayerAction :: Show DelayerAction
  where
  show Loading = "Loading"
  show (Loaded s) = "(Loaded " <> show s <> ")"

type State =
  { counter :: Int
  , delayer :: Maybe String
  }

initialState :: State
initialState =
  { counter: 0
  , delayer: Just "Click the button to launch a delayed request."
  }

rootReducer :: Either Int DelayerAction -> State -> Aff State
rootReducer act s =
  trace "root reducer" $ \_ ->
  trace (show s)       $ \_ ->
  trace (show act)     $ \_ ->
  case act of
    Left i  -> pure $ s { counter = i }
    Right da -> (s { delayer = _ }) <$> delayerReducer da s.delayer

delayerReducer :: DelayerAction -> Maybe String -> Aff (Maybe String)
delayerReducer (Loaded s) _ = pure $ Just s
delayerReducer Loading _ = do
  delay (Milliseconds 1000.0)
  -- put (Loaded "Delayed request completed.")
  pure Nothing
