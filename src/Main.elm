module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, decodeString, int, string, list)
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

type Status
  = Failure
  | Loading
  | Success

type alias Model =
  { quests : List Quest
  , selectedQuest : Int
  , status : Status
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model
      []
      0
      Loading
  , getQuests
  )

-- UPDATE

type Msg
  = LoadQuests
  | GotQuests (Result Http.Error (List Quest))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LoadQuests ->
          ( { model | status = Success }
          , Cmd.none
          )

    GotQuests result ->
      case result of
        Ok quests ->
          ( { model 
            | quests = quests 
            , status = Success
            }
          , Cmd.none
          )

        Err e ->
          let
              a = Debug.log "err" e
          in
          ( { model | status = Failure }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Quests" ]
    , div []
      (viewQuestList model.quests)
    ]

viewQuestList : List Quest -> (List (Html Msg))
viewQuestList quests =
  List.map viewQuest quests

viewQuest : Quest -> Html Msg
viewQuest quest =
  div []
    [ h3 [] [ text ("#" ++ String.fromInt quest.id ++ ": " ++ quest.name ) ]
    , ul []
      [ li [] [ text (String.fromInt quest.id) ]
      , li [] [ text quest.diet ]
      , li [] [ text quest.responded ]
      , li [] [ text quest.edited ]
      , li [] [ text quest.type_name ]
      , li [] [ text quest.group_name ]
      ]
    ]

-- HTTP

getQuests : Cmd Msg
getQuests =
  Http.get
    { url = "http://127.0.0.1:5000/api/quests?group=1"
    , expect = Http.expectJson GotQuests questListDecoder
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

questListDecoder : Decoder (List Quest)
questListDecoder =
  Json.Decode.list questDecoder


