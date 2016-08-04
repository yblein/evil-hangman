-- TODO: randomize the word list so that results are less predictible
-- TODO: remove debug logs
-- TODO: display a solution after losing

port module Main exposing (..)

import Array exposing (Array)
import Char
import Debug exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import List
import Random
import String exposing (lines, uncons)
import Task



main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }


port focus : String -> Cmd msg



-- MODEL


minLetters = 3
maxLetters = 8
nbGuesses = 10


type alias Model =
  { dictionnary : List String
  , state : State
  }


type State
  = Loading
  | Ready
  | Playing GameData
  | GameOver Bool


type alias GameData =
  { words : List String
  , solution : Array (Maybe Char)
  , previousGuesses : List Char
  , nbRemGuesses : Int
  }


init : (Model, Cmd Msg)
init =
  ( Model [] Loading
  , Task.perform FetchFail FetchSucceed <| Http.getString "/static/dict.txt"
  )



-- UPDATE


type Msg
  = FetchSucceed String
  | FetchFail Http.Error
  | Start
  | NewNbLetters Int
  | KeyPressed Char.KeyCode


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model.state) of
    (FetchSucceed contents, _) ->
      (Model (lines contents) Ready, Cmd.none)

    (Start, _) ->
      (model, Random.generate NewNbLetters (Random.int minLetters maxLetters))

    (NewNbLetters n, _) ->
      (start model n, focus "input")

    (KeyPressed keyCode, Playing gameData) ->
      let
        c = Char.toLower <| Char.fromCode keyCode
      in
        if Char.isLower c && not (List.member c gameData.previousGuesses) then
          let
            newGameData = handleChar gameData c
          in
            if List.all isJust <| Array.toList newGameData.solution then
              ({ model | state = GameOver True }, Cmd.none)
            else if newGameData.nbRemGuesses == 0 then
              ({ model | state = GameOver False }, Cmd.none)
            else
              ({ model | state = Playing newGameData }, Cmd.none)
        else
          (model, Cmd.none)

    _ ->
      (model, Cmd.none)


start : Model -> Int -> Model
start model nbLetters =
  let
    initialWords = List.filter (\w -> String.length w == nbLetters) model.dictionnary
    initialSolution = Array.repeat nbLetters Nothing
    gameData = GameData initialWords initialSolution [] nbGuesses
  in
    { model | state = Playing gameData }


handleChar : GameData -> Char -> GameData
handleChar gameData char =
  case minimumBy minInformation <| log "families" (computeWordFamilies gameData.words char) of
    Nothing -> gameData
    Just (indexes, newWords) ->
      let
        newSolution = updateSolution indexes char gameData.solution
        correct = newSolution /= gameData.solution
        newNbRemGuesses = gameData.nbRemGuesses - (if correct then 0 else 1)
        newPreviousGuesses = char :: gameData.previousGuesses
      in
        GameData newWords newSolution newPreviousGuesses newNbRemGuesses


computeWordFamilies : List String -> Char -> List (List Int, List String)
computeWordFamilies words char =
  let
    insertWord word families =
      let
        indexes = String.indexes (String.fromChar char) word
      in
        case Dict.get indexes families of
          Just f -> Dict.insert indexes (word :: f) families
          Nothing -> Dict.insert indexes [word] families
  in
    Dict.toList <| List.foldl insertWord Dict.empty words


{-| Order word families by minimum information, i.e., largest set of words and
 minimum set of new letters.
-}
minInformation : (List Int, List String) -> (List Int, List String) -> Order
minInformation (p1, f1) (p2, f2) =
  case compare (List.length f1) (List.length f2) of
    GT -> LT
    LT -> GT
    EQ -> compare (List.length p1) (List.length p2)


updateSolution : List Int -> Char -> Array (Maybe Char) -> Array (Maybe Char)
updateSolution indexes char =
  Array.indexedMap <| \i mc -> if List.member i indexes then Just char else mc


minimumBy : (a -> a -> Order) -> List a -> Maybe a
minimumBy f = List.sortWith f >> List.head


isJust m =
  case m of
    Nothing -> False
    Just _ -> True



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [text "Evil Hangman"]
    , div []
      (case model.state of
        Loading -> [ text "Loading..." ]
        Ready -> [ button [ onClick Start] [ text "Start" ] ]
        Playing gameData ->
          [ div [] (viewCurrentSolution gameData.solution)
          , s [] (List.map (text << String.fromChar) <| List.reverse gameData.previousGuesses)
          , br [] []
          , text "Remaning guesses: "
          , text <| toString <| gameData.nbRemGuesses
          , input [ onKeyPress KeyPressed, style [ ("opacity", "0") ] ] []
          ]
        GameOver hasWon ->
          [ text (if hasWon then "You won!" else "You lost!")
          , br [] []
          , button [ onClick Start] [ text "Restart" ]
          ]
      )
    ]


viewCurrentSolution : Array (Maybe Char) -> List (Html Msg)
viewCurrentSolution s =
  let
    viewMaybeChar mc =
      case mc of
        Just c -> text <| String.fromChar c
        Nothing -> text "-"
  in
    Array.toList s |> List.map viewMaybeChar


onKeyPress : (Int -> msg) -> Attribute msg
onKeyPress tagger =
  on "keypress" (Json.map tagger keyCode)
