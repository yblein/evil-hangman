-- TODO: remove debug logs

port module EvilHangman exposing (..)

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
import Random.Extra
import String exposing (lines, uncons)
import Task



main : Program Never
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
  | Won
  | GameOver String


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
  | Randomized (Int, List String)
  | KeyPressed Char.KeyCode
  | Solution (Maybe String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model.state) of
    (FetchSucceed contents, _) ->
      (Model (lines contents) Ready, Cmd.none)

    (Start, _) ->
      let
        genNbLetters = Random.int minLetters maxLetters
        genDict = shuffle model.dictionnary
      in
        (model, Random.generate Randomized <| Random.pair genNbLetters genDict)

    (Randomized (n, dict), _) ->
      (start { model | dictionnary = dict } n, focus "input")

    (KeyPressed keyCode, Playing gameData) ->
      let
        c = Char.toLower <| Char.fromCode keyCode
      in
        if Char.isLower c && not (List.member c gameData.previousGuesses) then
          let
            newGameData = handleChar gameData c
          in
            if List.all isJust <| Array.toList newGameData.solution then
              ({ model | state = Won }, Cmd.none)
            else if newGameData.nbRemGuesses == 0 then
              (model, Random.generate Solution <| Random.Extra.sample gameData.words)
            else
              ({ model | state = Playing newGameData }, Cmd.none)
        else
          (model, Cmd.none)

    (Solution (Just s), _) ->
      ({ model | state = GameOver s }, Cmd.none)

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


isJust : Maybe a -> Bool
isJust m =
  case m of
    Nothing -> False
    Just _ -> True


shuffle : List a -> Random.Generator (List a)
shuffle xs =
  let
    genFloats = Random.list (List.length xs) (Random.float 0 1)
    sortBy fs = List.map2 (,) fs xs |> List.sortBy fst |> List.map snd
  in
    Random.map sortBy genFloats



-- VIEW


view : Model -> Html Msg
view model =
  div [ id "wrapper" ]
    [ h1 [] [text "Evil Hangman"]
    , p [] [text "This is an implementation of the classic Hangman game. It features only simple and common english words like \"boat\" or \"forest\"."]
    , div []
        (case model.state of
          Loading -> [ text "Loading..." ]
          Ready -> [ button [ onClick Start ] [ text "Play" ] ]
          Playing gameData ->
            [ p [] [ viewCurrentSolution gameData.solution ]
            , p [] [ s [] [ makeCharList <| List.reverse <| failedAttemps gameData ] ]
            , p []
              [ text "You have "
              , text <| toString <| gameData.nbRemGuesses
              , text <| " remaining guess" ++ (if gameData.nbRemGuesses == 1 then "" else "es") ++ "."
              ]
            , input [ onKeyPress KeyPressed, style [ ("opacity", "0") ] ] []
            ]
          GameOver solution ->
            [ p [] [ text "Game over >:)" ]
            , p [] [ text <| "The correct word was \"" ++ solution ++ "\"." ]
            , button [ onClick Start] [ text "Replay" ]
            ]
          Won ->
            [ p [] [ text "You won!" ]
            , button [ onClick Start] [ text "Replay" ]
            ]
        )
    ]


viewCurrentSolution : Array (Maybe Char) -> Html Msg
viewCurrentSolution =
  makeCharList << List.map (Maybe.withDefault '_') << Array.toList


makeCharList : List Char -> Html Msg
makeCharList =
  ul [] << List.map (\s -> li [] [ text <| String.fromChar s ])


failedAttemps : GameData -> List Char
failedAttemps gameData =
  let
    correctAttemps = Array.toList gameData.solution
  in
    List.filter (\c -> not <| List.member (Just c) correctAttemps) gameData.previousGuesses


onKeyPress : (Int -> msg) -> Attribute msg
onKeyPress tagger =
  on "keypress" (Json.map tagger keyCode)
