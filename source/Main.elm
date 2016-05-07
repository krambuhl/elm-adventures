module Main where

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode exposing (string)


type Action
    = NoOp
    | AddHistory
    | RemoveHistory
    | EditHistory


type alias Model = 
    { events: List Event
    , cursor: String }


type alias Event = 
    { uid: Int
    , title: String
    , href: String }


updateEvent : Action -> Event -> Event
updateEvent action event = 
    case action of
        NoOp -> event
        AddHistory -> 
            { event | 
                uid = event.uid + 1 }   
        RemoveHistory -> event
        EditHistory -> event


questionView : String -> Html
questionView question = 
    div [ class "question"
        , attribute "aria-role" "question" ]
        [ span 
            [ ]
            [ text question ]]

main = 
    div [ class "app" ] 
        [ questionView "What's the history around here?" 
        , questionView "Tell a story." ]