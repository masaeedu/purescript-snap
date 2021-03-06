module Examples.TodoMVC.State where

import Prelude

import Data.Lens as L
import Data.Lens.Record (prop)
import Data.Lens.Record.Extra (extractedBy)
import Data.Maybe (Maybe(..), isJust)
import Data.Profunctor.Optics (Transactional, isDirty)
import Data.Symbol (SProxy(..))
import Snap.React.Component (InputState)

-- TODO:
-- 1. Set up routing stuff

-- #### STATE

-- The state corresponding to a todo item
type Todo =
  { done :: Boolean
  , hovered :: Boolean
  , value :: String
  , modification :: Maybe String
  }

type Todos = Array Todo

data Filter = All | Active | Completed
derive instance eqFilter :: Eq Filter

instance showFilter :: Show Filter where
  show All = "All"
  show Active = "Active"
  show Completed = "Completed"

shouldHide :: Filter -> Todo -> Boolean
shouldHide All       = const false
shouldHide Active    = _.done
shouldHide Completed = not _.done

className :: Filter -> Todo -> String
className f t =
     (if isJust t.modification then " editing "   else "")
  <> (if t.done                then " completed " else "")
  <> (if (shouldHide f t)    then " hidden "    else "")

type App =
  { newTodo :: InputState
  , todos :: Todos
  , filter :: Filter
  }

-- Create a new todo
createTodo :: String -> Todo
createTodo =
  { value: _
  , done: false
  , hovered: false
  , modification: Nothing
  }

defaultNewTodo :: InputState
defaultNewTodo = { focused: true, value: "" }

-- Initial application state consists of three components
initialState :: App
initialState = { newTodo: defaultNewTodo, todos: [], filter: All }

proxies =
  { done:         SProxy :: _ "done"
  , hovered:      SProxy :: _ "hovered"
  , modification: SProxy :: _ "modification"
  , value:        SProxy :: _ "value"
  , focused:      SProxy :: _ "focused"
  , newTodo:      SProxy :: _ "newTodo"
  , todos:        SProxy :: _ "todos"
  , filter:       SProxy :: _ "filter"
  }

_state :: L.Lens' Todo (Transactional String)
_state = extractedBy { value: SProxy, modification: SProxy }

_dirty :: L.Lens' Todo Boolean
_dirty = isDirty >>> _state

_done :: L.Lens' Todo Boolean
_done    = prop proxies.done

_hovered :: L.Lens' Todo Boolean
_hovered = prop proxies.hovered

_modification :: L.Lens' Todo (Maybe String)
_modification = prop proxies.modification

_value :: L.Lens' Todo String
_value   = prop proxies.value

_focused :: L.Lens' InputState Boolean
_focused = prop proxies.focused

_newTodo :: L.Lens' App InputState
_newTodo = prop proxies.newTodo

_todos :: L.Lens' App Todos
_todos   = prop proxies.todos

_filter :: L.Lens' App Filter
_filter  = prop proxies.filter
