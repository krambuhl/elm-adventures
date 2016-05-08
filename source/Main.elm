module Main where

--import StartApp

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Signal exposing (Signal, Address, send)
import Json.Encode exposing (string)
import Json.Decode as Json


-- DATA TYPES

type alias AppState = 
  { events: List Event
  , total: Int }


type alias Event = 
  { uid: Int
  , title: String
  , editing: Bool }


emptyModel : AppState
emptyModel =
  { events = []
  , total = 0 }

newEvent : Int -> String -> Event
newEvent id name =
  { uid = id
  , title = name
  , editing = False }


--[ eventView "What's the history around here?" 
--, eventView "Tell a story." ]

-- UPDATES

type Action
  = NoOp
  | AddEvent String
  | RemoveEvent Int
  | EditingEvent Int Bool
  | EditEvent Int String
  | StopEditingEvents


update : Action -> AppState -> AppState
update action model = 
  case action of
    NoOp -> model
    
    AddEvent title ->
      { model | 
          total = model.total + 1,
          events = model.events ++ [newEvent (model.total + 1) title] }
    
    RemoveEvent id -> 
      { model | events = List.filter (\ev -> ev.uid /= id) model.events}
    
    EditingEvent id isEditing ->
      let updateEvent ev = if ev.uid == id then { ev | editing = isEditing } else ev
      in { model | events = List.map updateEvent model.events }

    StopEditingEvents -> 
      let updateEvent ev = { ev | editing = False }
      in { model | events = List.map updateEvent model.events }

    EditEvent id newTitle -> 
      let updateEvent ev = if ev.uid == id then { ev | title = newTitle } else ev
      in { model | events = List.map updateEvent model.events }



-- VIEWS

eventView : Address Action -> Event -> Html
eventView address event = 
  div 
    [ classList [ 
      ("event", True), 
      ("is-editing", event.editing) ] ]
    [ if event.editing 
      then (eventEditView address event) 
      else (eventDispayView address event) ]

eventDispayView : Address Action -> Event -> Html
eventDispayView address event =
  span 
    [ onClick address (EditingEvent event.uid True) ]
    [ text (toString event.uid)
    , text ". "
    , text event.title ]


eventEditView : Address Action -> Event -> Html
eventEditView address event =
  let 
    options = { preventDefault = True, stopPropagation = False }
    decoder = (Json.customDecoder keyCode (\k ->
      if List.member k [13]
      then Ok k
      else Err "not handling that key"))
  in
  span 
    [ ]
    [ input
      [ value event.title
      , on "input" targetValue (Signal.message address << EditEvent event.uid) 
      , onWithOptions "keydown" options decoder (\code -> Signal.message address (EditingEvent event.uid False)) ] 
      [ ] ]


-- MAIN

view : Address Action -> AppState -> Html
view address model =
  div 
    [ class "app" ]  
    [ div 
      []
      [ h1 
        [ onClick address (AddEvent "Tell a story.") ]
        [ text "Click for a story" ] ]
    , div 
      [] 
      (List.map (eventView address) model.events)]


model : Signal AppState
model =
  Signal.foldp update emptyModel actions.signal


main : Signal Html
main = 
    Signal.map (view actions.address) model


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp