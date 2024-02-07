module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, map2, map3, map5, field, int, string)
import Json.Encode as JE

-- MAIN

main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


-- MODEL

type alias Model = 
  {
    user : User
  , teams : List Team
  }

type alias User =
  {
    name : String
  , password : String
  , token: Maybe String
  }

type alias LoginResponse = 
  {
    acess_token : String      -- TODO fix this typo!
  , expires_in : Int
  , token_type: String
  }

type alias Teams = List Team

type alias Team =
  {
    teamGuid: String,
    name: String,
    location: String,
    owner: Owner,
    players: List Player
  }

type alias Player = 
  {
    playerGuid: String,
    name: String,
    seasonSalary: Maybe Int
  }

type alias Owner =
  {
    ownerGuid: String,
    name: String
  }

baseUrl : String
baseUrl = "http://localhost:5130"

init : () -> (Model, Cmd Msg)
init _ =
  let
    emptyModel = Model (User "" "" Nothing) []
  in
    (emptyModel, 
      updateTeams emptyModel)


-- UPDATE

type Msg
  = Name String
  | Password String
  | Submit
  | LoggedIn (Result Http.Error LoginResponse)
  | GotTeams (Result Http.Error Teams)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Name name ->
      let
        oldUser = model.user
        newUser = {oldUser | name = name}
      in
        ({ model | user = newUser }, Cmd.none)

    Password password ->
      let
        oldUser = model.user
        newUser = {oldUser | password = password }
      in
        ({ model | user = newUser }, Cmd.none)

    Submit ->
      ( model,
        Http.post 
          {
            url = baseUrl ++ "/auth/token"
          , body = Http.jsonBody <| loginRequestEncoder model
          , expect = Http.expectJson LoggedIn loginResponseDecoder
          })

    LoggedIn response ->
      case response of
        Ok body ->
          let
            token = body.acess_token
            oldUser = model.user
            newUser = {oldUser | token = Just token}
            newModel = {model | user = newUser }
          in
            (newModel
              , updateTeams newModel)

        Err _ ->
          Debug.todo "branch 'Err _' not implemented"

    GotTeams result ->
      case result of
        Ok teams ->
          (
            { model | teams = teams }
            , Cmd.none
          )

        Err err ->
          let
            _ = Debug.log "Error" err
          in
            (model, Cmd.none)


loginResponseDecoder : Decoder LoginResponse
loginResponseDecoder = 
  map3 LoginResponse
    (field "acess_token" string)
    (field "expires_in" int)
    (field "token_type" string)

loginRequestEncoder : Model -> JE.Value
loginRequestEncoder model =
  JE.object 
    [ ("username", JE.string model.user.name )
    , ("password", JE.string model.user.password )
    ]

playerDecoder : Decoder Player
playerDecoder =
  map3 Player
    (field "playerGuid" string)
    (field "name" string)
    (field "seasonSalary" (Json.Decode.nullable int))

teamDecoder : Decoder Team
teamDecoder =
  map5 Team
    (field "teamGuid" string)
    (field "name" string)
    (field "location" string)
    (field "owner" ownerDecoder)
    (field "players" (Json.Decode.list playerDecoder))

teamsResponseDecoder : Decoder Teams
teamsResponseDecoder = 
  Json.Decode.at ["teams"] (Json.Decode.list teamDecoder)

ownerDecoder : Decoder Owner
ownerDecoder =
  map2 Owner
    (field "ownerGuid" string)
    (field "name" string)

updateTeams : Model ->  Cmd Msg
updateTeams model =
  let
    token = Maybe.map 
      (\t -> [ Http.header "Authorization" ("Bearer " ++ t) ])
      model.user.token
                
    request = 
          { method = "GET"
          , headers = Maybe.withDefault [] token
          , url = baseUrl ++ "/busybody/league"
          , body = Http.emptyBody
          , expect = Http.expectJson GotTeams teamsResponseDecoder
          , timeout = Nothing
          , tracker = Nothing
          }
        |> Http.request
  in
    request

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Username" model.user.name Name
    , viewInput "password" "Password" model.user.password Password
    , button [ onClick Submit ] [ text "Login" ]
    , hr [] []
    , div [] (List.map viewTeam model.teams)
    ]

viewTeam : Team -> Html msg
viewTeam team =
  div [] [
    div [] [text ("The " ++ team.location ++ " " ++ team.name)],
    div [] [text ("Owner: " ++ team.owner.name)],
    div [] [
      (text "Players:"),
      ul [] (List.map viewPlayer team.players)
    ]
  ]

viewPlayer : Player -> Html msg
viewPlayer player =
  let
    salary = case player.seasonSalary of
      Just value -> "$" ++ String.fromInt value
      Nothing -> "--"
  in
    ul [] [text (player.name ++ " (" ++ salary ++ ")")]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []
