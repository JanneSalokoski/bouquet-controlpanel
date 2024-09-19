module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, decodeString, int, string)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

type alias Quest =
  { id : Int 
  , name : String
  , email : String
  , diet : String
  , responded : String
  , edited : String
  , type_id : Int
  , type_name : String
  , group_id : Int
  , group_name : String
  }

type Model
  = Failure
  | Loading
  | Success Quest


init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getQuest)

-- UPDATE

type Msg
  = LoadQuests
  | GotQuest (Result Http.Error Quest)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LoadQuests ->
      (Loading, getQuest)

    GotQuest result ->
      case result of
        Ok quest ->
          (Success quest, Cmd.none)

        Err e ->
          let
              a = Debug.log "err" e
          in
          (Failure, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Quests" ]
    , viewQuest model
    ]

viewQuest : Model -> Html Msg
viewQuest model =
  case model of
    Failure ->
      div []
        [ text "Could not load anything :("
        , button [ onClick LoadQuests ] [ text "Try again!" ]
        ]

    Loading ->
      text "Loading..."

    Success quest ->
      div []
        [ button [ onClick LoadQuests ] [ text "Load more!" ]
        , p [] [ text quest.name ]
        ]

-- HTTP

getQuest : Cmd Msg
getQuest =
  Http.get
    { url = "http://127.0.0.1:5000/api/quest/1"
    , expect = Http.expectJson GotQuest questDecoder
    }

questDecoder : Decoder Quest
questDecoder =
  Json.Decode.succeed Quest
    |> required "id" int
    |> required "name" string
    |> required "email" string
    |> required "diet" string
    |> required "responded" string
    |> required "edited" string
    |> required "type_id" int
    |> required "type_name" string
    |> required "group_id" int
    |> required "group_name" string

