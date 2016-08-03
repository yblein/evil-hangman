-- TODO: focus input field automatically when starting the game (see TodoMVC)
-- TODO: move the data of the `playing` state into a record
-- TODO: randomize the word list so that results are less predictible
-- TODO: remove debug logs

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
  | Playing (List String) (Array (Maybe Char)) (List Char) Int
  | GameOver Bool


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
      (Model (List.sortBy String.length <| lines contents) Ready, Cmd.none)

    (Start, _) ->
      (model, Random.generate NewNbLetters (Random.int minLetters maxLetters))

    (NewNbLetters n, _) ->
      (start model n, Cmd.none)

    (KeyPressed keyCode, Playing currentWords currentSolution prevGuesses remGuesses) ->
      let
        c = Char.toLower <| Char.fromCode keyCode
      in
        if Char.isLower c && not (List.member c prevGuesses) then
          let
            (newWords, newSolution) = handleChar currentWords currentSolution c
            newRemGuesses = if newSolution /= currentSolution then remGuesses else remGuesses - 1
          in
            if List.all isJust <| Array.toList newSolution then
              ({ model | state = GameOver True }, Cmd.none)
            else if newRemGuesses == 0 then
              ({ model | state = GameOver False }, Cmd.none)
            else
              ({ model | state = Playing newWords newSolution (c :: prevGuesses) newRemGuesses }, Cmd.none)
        else
          (model, Cmd.none)

    _ ->
      (model, Cmd.none)


isJust m =
  case m of
    Nothing -> False
    Just _ -> True


start : Model -> Int -> Model
start model nbLetters =
  let
    initialWords = List.filter (\w -> String.length w == nbLetters) model.dictionnary
    initialSolution = Array.repeat nbLetters Nothing
  in
    { model | state = Playing initialWords initialSolution [] nbGuesses }


handleChar : List String -> Array (Maybe Char) -> Char -> (List String, Array (Maybe Char))
handleChar words currentSolution c =
  let
    insertWord w families =
      let
        idxs = indexes c w
      in
        case Dict.get idxs families of
          Just f -> Dict.insert idxs (w :: f) families
          Nothing -> Dict.insert idxs [w] families
    families = List.foldl insertWord Dict.empty words
    result = Dict.toList (log "families" families) |> List.sortWith minInformation |> List.head
    updateSolution idxs i mc = if List.member i idxs then Just c else mc
  in
    case result of
      Nothing -> ([], currentSolution)
      Just (idxs, newWords) -> (newWords, Array.indexedMap (updateSolution idxs) currentSolution)


{-| Returns the list of occurence indexes of a char in a given string.
-}
indexes : Char -> String -> List Int
indexes c s =
  let
    rec s i =
      case uncons s of
        Nothing -> []
        Just (x, xs) ->
          if x == c then
            i :: rec xs (i + 1)
          else
            rec xs (i + 1)
  in
    rec s 0


{-| Order word families by minimum information, i.e., largest set of words and
 minimum set of new letters.
-}
minInformation : (List Int, List String) -> (List Int, List String) -> Order
minInformation (p1, f1) (p2, f2) =
  case compare (List.length f1) (List.length f2) of
    GT -> LT
    LT -> GT
    EQ -> compare (List.length p1) (List.length p2)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [text "Evil Hangman"]
    , div []
      (case model.state of
        Loading -> [ text "Loading..." ]
        Ready -> [ button [ onClick Start] [ text "Start" ] ]
        Playing _ currentSolution prevGuesses remGuesses ->
          [ div [] (viewCurrentSolution currentSolution)
          , s [] (List.map (String.fromChar >> text) <| List.reverse prevGuesses)
          , br [] []
          , text "Remaning guesses: "
          , text <| toString <| remGuesses ]
        GameOver hasWon ->
          [ text (if hasWon then "You won!" else "You lost!")
          , br [] []
          , button [ onClick Start] [ text "Restart" ]
          ]
      )
    , input [ onKeyPress KeyPressed, style [ ("opacity", ".5") ] ] []
    , text <| String.join " "
      (case model.state of
        Playing currentWords _ _ _ -> [] -- currentWords
        _ -> []
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
