module Examples.Reducer.UI where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe)
import Debug.Trace (trace)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Examples.Reducer.State (DelayerAction(..), State, rootReducer)
import React.Basic (JSX)
import React.Basic.DOM as R
import Snap.React.Component (counter, (|-), (|<))
import Snap.React.Component as RC
import Snap.SYTC.Component (Cmp, Cmp', handleM)
import Snap.SYTC.Component as C

app :: Cmp' Aff JSX State
app = handleM rootReducer $ C.ado
  cntr <- fromEffCmp $ counter # C.dimap _.counter Left
  dlyr <- delayer # C.dimap _.delayer Right
  in
  R.div
  |< [ cntr
     , dlyr
     ]

delayer :: Cmp Aff JSX (Maybe String) DelayerAction
delayer = C.ado
  load <- fromEffCmp $ RC.button # C.rmap (const Loading)
  txt  <- RC.text # C.lcmap (fromMaybe "Loading...")
  in
  R.div
  |< [ load |- R.text "Click Me"
     , txt
     ]

fromEffCmp :: forall v s u. Cmp Effect v s u -> Cmp Aff v s u
fromEffCmp = C.contraHoist launchAff_
