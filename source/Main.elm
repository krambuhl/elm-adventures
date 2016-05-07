module Main where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)

import Signal exposing (Signal, Address, send)
import Json.Encode exposing (string)


-- DATA TYPES

type alias Model = 
  { events: List Event
  , cursor: String }


type alias Event = 
  { uid: Int
  , title: String }


emptyModel : Model
emptyModel =
  { events = []
  , cursor = "" }

newEvent: Int -> String -> Event
newEvent id name =
  { uid = id
  , title = name }


--[ eventView "What's the history around here?" 
--, eventView "Tell a story." ]

-- UPDATES

type Action
  = NoOp
  | AddEvent Int String
  | RemoveEvent Int
  | EditEvent


update : Action -> Model -> Model
update action model = 
  case action of
    NoOp -> model
    
    AddEvent id title ->
      { model | events = model.events ++ [newEvent id title] }
    
    RemoveEvent id -> 
      { model | events = List.filter (\ev -> ev.uid /= id) model.events}
    
    EditEvent -> model



-- VIEWS

eventView : Address Action -> Event -> Html
eventView address events = 
  div 
    [ class "question" 
    , onClick address (RemoveEvent events.uid) ]
    [ span 
      [ ]
      [ text (toString events.uid)
      , text ". "
      , text events.title ] ]


-- MAIN

view : Address Action -> Model -> Html
view address model =
  div 
    [ class "app" ]  
    [ div []
      [ h1 
        [ onClick address (AddEvent (List.length model.events) "Tell a story.") ]
        [ text "Click for a story" ] ]
    , div [] 
      (List.map (eventView address) model.events)]


model : Signal Model
model =
  Signal.foldp update emptyModel actions.signal


main : Signal Html
main = 
    Signal.map (view actions.address) model


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp