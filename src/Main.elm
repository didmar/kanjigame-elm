module Main exposing (..)

import Array exposing (..)
import Browser
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (checked, disabled, href, name, placeholder, src, style, target, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Http
import Json.Decode as Json
import Random
import Time exposing (every)
import Url
import Url.Builder as UB
import Url.Parser as UP
import Url.Parser.Query as UPQ



-- CONFIG


apiBaseURL : String
apiBaseURL =
    "http://127.0.0.1:9000"


maxHP : Int
maxHP =
    10


maxTimer : Int
maxTimer =
    30



-- MAIN


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


{-| String composed ONLY of hiragana characters
-}
type alias Hiragana =
    String


{-| String composed of a single kanji character
-}
type alias Kanji =
    String


type alias KanjiEntry =
    { kanji : Kanji
    , meaning : String
    , jlpt : Int
    }


{-| Required only to initialize the model, will be replaced by a random one
-}
defaultKanji : Kanji
defaultKanji =
    "一"


{-| Required only to initialize the model, will be replaced by a random one
-}
defaultKanjiEntry : KanjiEntry
defaultKanjiEntry =
    { kanji = defaultKanji, meaning = "", jlpt = 1 }


kanjiEntriesToDict : List KanjiEntry -> Dict Kanji KanjiEntry
kanjiEntriesToDict kanjiEntries =
    Dict.fromList (List.map (\ke -> ( ke.kanji, ke )) kanjiEntries)


{-| Word selected by the Jamdict API
-}
type alias ValidWord =
    String


{-| Entry from the dictionary, conform to the game rules
-}
type alias WordEntry =
    { word : ValidWord
    , kana : String
    , meaning : String
    , kanjis : List Kanji
    }


type alias WordEntries =
    List WordEntry


type alias Input =
    { romaji : String
    , converted : Result String Hiragana
    }


emptyInput : Input
emptyInput =
    { romaji = "", converted = Err "" }


type alias Timer =
    { active : Bool
    , value : Int
    , maxValue : Int
    }


initTimer : Timer
initTimer =
    { active = True, value = maxTimer, maxValue = maxTimer }


type alias Model =
    { kanjiToMatch : KanjiEntry
    , input : Input
    , wordMatches : Array WordEntry
    , selectedIndex : Maybe Int
    , history : WordEntries
    , message : Maybe String
    , kanjis : Dict Kanji KanjiEntry
    , candidateKanjis : List Kanji
    , unseenKanjis : List Kanji
    , jokerWord : Maybe WordEntry
    , hp : Int
    , timer : Timer
    , params : Params
    , score : Int
    }


initModel : Params -> Model
initModel params =
    { kanjiToMatch = defaultKanjiEntry
    , input = emptyInput
    , wordMatches = Array.empty
    , selectedIndex = Nothing
    , history = []
    , message = Nothing
    , kanjis = Dict.empty
    , candidateKanjis = []
    , unseenKanjis = []
    , jokerWord = Nothing
    , hp = maxHP
    , timer = initTimer
    , params = params
    , score = 0
    }


type alias Params =
    { minJLPTLevel : Int }


init : String -> ( Model, Cmd Msg )
init query =
    let
        model =
            initModel (locationHrefToParams query)
    in
    ( model
    , getKanjis model
    )


defaultParams : Params
defaultParams =
    { minJLPTLevel = 3 }


locationHrefToParams : String -> Params
locationHrefToParams locationHref =
    Url.fromString locationHref
        |> Maybe.andThen (\url -> UP.parse queryArgsParser url)
        |> Maybe.map (\minJLPTLevel -> { minJLPTLevel = Maybe.withDefault 3 minJLPTLevel })
        |> Maybe.withDefault defaultParams


queryArgsParser : UP.Parser (Maybe Int -> a) a
queryArgsParser =
    UP.query (UPQ.int "jlpt")



-- UPDATE


type Msg
    = GotKanjis (Result Http.Error (List KanjiEntry))
    | PickedKanji Kanji
    | GotKanjiEntry (Result Http.Error KanjiEntry)
    | GotJokerWord (Result Http.Error (Maybe WordEntry))
    | UpdatedInput String
    | GotConverted (Result Http.Error (Result String Hiragana))
    | SubmittedInput
    | GotWordMatches (Result Http.Error WordEntries)
    | WordSelected String
    | SelectionConfirmed
    | Ticked Time.Posix


withDebugLog : Msg -> Msg
withDebugLog message =
    case message of
        Ticked _ ->
            message

        _ ->
            Debug.log "Msg" message


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case withDebugLog message of
        GotKanjis result ->
            case result of
                Ok kanjiEntries ->
                    let
                        candidateKanjis =
                            List.filter (\ke -> ke.jlpt >= model.params.minJLPTLevel) kanjiEntries
                                |> List.map .kanji

                        newModel =
                            { model
                                | kanjis = kanjiEntriesToDict kanjiEntries
                                , candidateKanjis = candidateKanjis
                                , unseenKanjis = candidateKanjis
                            }
                    in
                    ( newModel
                    , drawKanji newModel
                    )

                Err _ ->
                    ( model, Cmd.none )

        PickedKanji newKanji ->
            ( updateKanjiToMatch model newKanji
            , Cmd.batch
                [ getJokerWord newKanji model.params.minJLPTLevel
                , getKanjiDetails newKanji
                ]
            )

        GotKanjiEntry result ->
            case result of
                Ok kanjiEntry ->
                    ( { model | kanjiToMatch = kanjiEntry }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        GotJokerWord result ->
            case result of
                Ok jokerWord ->
                    ( { model | jokerWord = Debug.log ("jokerWord for kanji " ++ model.kanjiToMatch.kanji) jokerWord }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        UpdatedInput romaji ->
            case romaji of
                "" ->
                    ( { model | input = emptyInput, message = Nothing }
                    , Cmd.none
                    )

                _ ->
                    ( updateRomaji model romaji
                    , romajiToHiragana romaji
                    )

        GotConverted result ->
            ( updateConverted model result
            , Cmd.none
            )

        SubmittedInput ->
            case model.input.converted of
                Ok hiragana ->
                    ( model
                    , searchWord hiragana model.kanjiToMatch.kanji
                    )

                Err "" ->
                    case model.hp of
                        1 ->
                            ( { model | message = Just "Can't give up with only 1 心 left !" }, Cmd.none )

                        _ ->
                            ( giveUp model, drawKanji model )

                _ ->
                    ( model, Cmd.none )

        GotWordMatches result ->
            case result of
                Ok wordMatches ->
                    case wordMatches of
                        [] ->
                            ( noMatch model
                            , Cmd.none
                            )

                        _ ->
                            case filterNotAlreadySubmitted model wordMatches of
                                [] ->
                                    ( { model | message = Just "Already used this word before !" }
                                    , Cmd.none
                                    )

                                w :: [] ->
                                    let
                                        newModel =
                                            addWord model w
                                    in
                                    ( newModel
                                    , drawKanji newModel
                                    )

                                ws ->
                                    ( { model | wordMatches = Array.fromList ws }
                                    , Cmd.none
                                    )

                Err _ ->
                    ( { model | input = emptyInput }
                    , Cmd.none
                    )

        WordSelected index ->
            ( { model | selectedIndex = String.toInt index }, Cmd.none )

        SelectionConfirmed ->
            case model.selectedIndex of
                Just index ->
                    case Array.get index model.wordMatches of
                        Just wordEntry ->
                            let
                                newModel =
                                    addWord model wordEntry
                            in
                            ( newModel, drawKanji newModel )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( { model | message = Just "You must select one of the words !" }, Cmd.none )

        Ticked newTime ->
            ( updateTimer model, Cmd.none )


filterNotAlreadySubmitted : Model -> List WordEntry -> List WordEntry
filterNotAlreadySubmitted model wordMatches =
    let
        notInHistory wordEntry =
            not (List.member wordEntry.word (List.map .word model.history))
    in
    List.filter notInHistory wordMatches


updateTimer : Model -> Model
updateTimer model =
    let
        upd timer =
            if timer.value > 1 then
                ( { timer | value = timer.value - 1 }, False )

            else
                ( { timer | value = timer.maxValue }, True )

        ( newTimer, reset ) =
            upd model.timer
    in
    if reset then
        { model | timer = newTimer, hp = max (model.hp - 1) 0 }

    else
        { model | timer = newTimer }


getKanjiDetails : Kanji -> Cmd Msg
getKanjiDetails kanji =
    getJson
        (UB.relative [ apiBaseURL, "kanji-details", kanji ] [])
        kanjiEntryDecoder
        GotKanjiEntry


kanjiEntryDecoder : Json.Decoder KanjiEntry
kanjiEntryDecoder =
    Json.map3 KanjiEntry
        (Json.field "kanji" Json.string)
        (Json.field "meaning" Json.string)
        (Json.field "jlpt" Json.int)


updateKanjiToMatch : Model -> Kanji -> Model
updateKanjiToMatch model newKanji =
    let
        newUnseenKanjis =
            removeFromList newKanji model.unseenKanjis
                |> (\ks ->
                        if List.isEmpty ks then
                            model.candidateKanjis

                        else
                            ks
                   )
    in
    { model
        | kanjiToMatch = Maybe.withDefault defaultKanjiEntry (Dict.get newKanji model.kanjis) -- TODO deal with this properly
        , unseenKanjis = newUnseenKanjis
    }


updateRomaji : Model -> String -> Model
updateRomaji model romaji =
    let
        upd input =
            { input | romaji = romaji }
    in
    { model | input = upd model.input, message = Nothing }


giveUp : Model -> Model
giveUp model =
    let
        newMsg =
            case model.jokerWord of
                Just wordEntry ->
                    "Could have used " ++ wordEntryToString wordEntry

                Nothing ->
                    "Did not have any joker..."
    in
    { model
        | input = emptyInput
        , jokerWord = Nothing
        , message = Just newMsg
        , hp = model.hp - 1
    }


getKanjis : Model -> Cmd Msg
getKanjis model =
    getJson
        (UB.relative [ apiBaseURL, "kanjis" ] [])
        kanjiEntriesDecoder
        GotKanjis


kanjisDecoder : Json.Decoder (List Kanji)
kanjisDecoder =
    Json.field "kanjis" (Json.list Json.string)


kanjiEntriesDecoder : Json.Decoder (List KanjiEntry)
kanjiEntriesDecoder =
    Json.field "kanjis" (Json.list kanjiEntryDecoder)


drawKanji : Model -> Cmd Msg
drawKanji model =
    let
        kanjis =
            case model.history of
                [] ->
                    model.unseenKanjis

                lastWord :: _ ->
                    candidateKanjisFromWord lastWord model.unseenKanjis
    in
    drawKanjiFromList kanjis


candidateKanjisFromWord : WordEntry -> List Kanji -> List Kanji
candidateKanjisFromWord word unseenKanjis =
    let
        kanjis =
            List.filter (\k -> List.member k unseenKanjis) word.kanjis
    in
    if List.isEmpty kanjis then
        unseenKanjis

    else
        kanjis


drawKanjiFromList : List Kanji -> Cmd Msg
drawKanjiFromList kanjis =
    Random.generate PickedKanji (kanjiGenerator kanjis)


getJokerWord : Kanji -> Int -> Cmd Msg
getJokerWord kanji minJLPTLevel =
    getJson
        (UB.relative
            [ apiBaseURL, "find-word-with-kanji", kanji ]
            [ UB.int "min_jlpt" minJLPTLevel ]
        )
        jokerWordDecoder
        GotJokerWord


jokerWordDecoder : Json.Decoder (Maybe WordEntry)
jokerWordDecoder =
    Json.field "result" (Json.maybe wordEntryDecoder)


kanjiGenerator : List Kanji -> Random.Generator Kanji
kanjiGenerator kanjis =
    let
        ( x, xs ) =
            case kanjis of
                [] ->
                    ( defaultKanji, [] )

                k :: ks ->
                    ( k, ks )
    in
    Random.uniform x xs


updateConverted : Model -> Result Http.Error (Result String Hiragana) -> Model
updateConverted model result =
    case result of
        Ok converted ->
            let
                upd input =
                    { input | converted = converted }
            in
            { model | input = upd model.input }

        Err _ ->
            model


noMatch : Model -> Model
noMatch model =
    { model
        | input = emptyInput
        , message = Just ("No match for " ++ showInput model ++ " !")
        , hp = model.hp - 1
    }


addWord : Model -> WordEntry -> Model
addWord model wordEntry =
    { model
        | history = wordEntry :: model.history
        , wordMatches = Array.empty
        , input = emptyInput
        , jokerWord = Nothing
        , score = model.score + scoreIncrease model wordEntry
    }


scoreIncrease : Model -> WordEntry -> Int
scoreIncrease model wordEntry =
    let
        kanjiScores =
            wordEntry.kanjis
                -- TODO deal properly with this
                |> List.map (\kanji -> Dict.get kanji model.kanjis |> Maybe.withDefault defaultKanjiEntry)
                |> List.map .jlpt
                -- 5 pts for N1, 1 pts for N5
                |> List.map (\jlpt -> 6 - jlpt)
    in
    List.foldl (+) 0 kanjiScores


romajiToHiragana : String -> Cmd Msg
romajiToHiragana romaji =
    case romaji of
        "" ->
            Cmd.none

        _ ->
            getJson
                (UB.relative [ apiBaseURL, "to-hiragana", romaji ] [])
                convertedDecoder
                GotConverted


convertedDecoder : Json.Decoder (Result String Hiragana)
convertedDecoder =
    Json.field "valid" Json.bool
        |> Json.andThen
            (\valid ->
                case valid of
                    True ->
                        Json.map Ok (Json.field "hiragana" Json.string)

                    False ->
                        Json.map Err (Json.field "hiragana" Json.string)
            )


searchWord : Hiragana -> Kanji -> Cmd Msg
searchWord word kanjiToMatch =
    getJson
        (UB.relative
            [ apiBaseURL, "lookup-words", word ]
            [ UB.string "kanji_to_match" kanjiToMatch ]
        )
        wordEntriesDecoder
        GotWordMatches


getJson : String -> Json.Decoder a -> (Result Http.Error a -> b) -> Cmd b
getJson url decoder toMsg =
    Http.request
        { method = "GET"
        , url = url
        , headers = []
        , expect = Http.expectJson toMsg decoder
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


wordEntriesDecoder : Json.Decoder WordEntries
wordEntriesDecoder =
    Json.field "valid_entries" (Json.list wordEntryDecoder)


wordEntryDecoder : Json.Decoder WordEntry
wordEntryDecoder =
    Json.map4 WordEntry
        (Json.field "word" Json.string)
        kanaDecoder
        meaningDecoder
        kanjisDecoder


meaningDecoder : Json.Decoder String
meaningDecoder =
    let
        senses =
            Json.field "senses" (Json.index 0 (Json.field "SenseGloss" (Json.list (Json.field "text" Json.string))))
    in
    Json.map (String.join " / ") senses


kanaDecoder : Json.Decoder String
kanaDecoder =
    Json.field "kana" (Json.index 0 (Json.field "text" Json.string))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.timer.active then
        every 1000 Ticked

    else
        Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    if model.hp > 0 then
        div []
            [ viewInfos model
            , viewKanjiToMatch model
            , viewInput model
            , viewMessage model
            , viewHint model
            , viewWordSelector model
            , viewHistory model
            ]

    else
        div []
            [ viewInfos model
            , div [ style "font-size" "8em" ] [ text "Game over !!!" ]
            , viewHistory model
            ]


viewInfos : Model -> Html Msg
viewInfos model =
    div []
        [ text
            ("心ｘ"
                ++ String.fromInt model.hp
                ++ " タイマ："
                ++ String.fromInt model.timer.value
                ++ " SCORE："
                ++ String.fromInt model.score
                ++ " 漢字："
                ++ String.fromInt (List.length model.unseenKanjis)
                ++ "／"
                ++ String.fromInt (List.length model.candidateKanjis)
                ++ (" (" ++ showJLPT model.params.minJLPTLevel ++ ")")
            )
        ]


viewKanjiToMatch : Model -> Html Msg
viewKanjiToMatch model =
    div [ style "min-height" "55pt" ]
        [ viewKanji model.kanjiToMatch.kanji
        , viewKanjiMeaning model.kanjiToMatch
        ]


viewKanji : Kanji -> Html Msg
viewKanji kanji =
    div
        [ style "font-size" "xxx-large"
        , style "float" "left"
        , style "margin-right" "10pt"
        ]
        [ text (kanji ++ "?") ]


viewKanjiMeaning : KanjiEntry -> Html Msg
viewKanjiMeaning kanjiEntry =
    div
        [ style "font-size" "medium" ]
        [ div
            []
            [ text kanjiEntry.meaning
            , viewKanjiDictLink kanjiEntry.kanji
            ]
        , div
            [ style "color" "gray" ]
            [ text (showJLPT kanjiEntry.jlpt) ]
        ]


showJLPT : Int -> String
showJLPT jlpt =
    "JLPT N" ++ String.fromInt jlpt


viewKanjiDictLink : Kanji -> Html Msg
viewKanjiDictLink kanji =
    a
        [ target "_blank"
        , href (UB.relative [ "https://jisho.org/search", kanji ++ " #kanji" ] [])
        ]
        [ img
            [ src "images/ext-link.svg"
            , style "height" "12pt"
            , style "margin-left" "5pt"
            ]
            []
        ]


viewInput : Model -> Html Msg
viewInput model =
    div
        []
        [ input
            [ placeholder ("Type a word with " ++ model.kanjiToMatch.kanji)
            , value model.input.romaji
            , onInput UpdatedInput
            , onEnter SubmittedInput
            , disabled (not (Array.isEmpty model.wordMatches))
            ]
            []
        , div
            [ style "font-size" "medium"
            , style "min-height" "18pt"
            ]
            [ text (showInput model) ]
        ]


viewMessage : Model -> Html Msg
viewMessage model =
    div
        [ style "font-size" "medium", style "min-height" "18pt", style "color" "red" ]
        [ text (Maybe.withDefault "" model.message) ]


viewHint : Model -> Html Msg
viewHint model =
    let
        elems =
            case model.jokerWord of
                Just jokerWord ->
                    [ u [] [ text "Hint:" ], text (" " ++ jokerWord.meaning) ]

                Nothing ->
                    []
    in
    div
        [ style "font-size" "medium" ]
        elems


viewHistory : Model -> Html Msg
viewHistory model =
    ul
        []
        (List.map viewWordEntry model.history)


viewWordEntry : WordEntry -> Html Msg
viewWordEntry wordEntry =
    li
        [ style "font-size" "medium"
        , style "height" "18pt"
        ]
        [ text (wordEntryToString wordEntry)
        , viewWordDictLink wordEntry
        ]


viewWordDictLink : WordEntry -> Html Msg
viewWordDictLink wordEntry =
    a
        [ target "_blank"
        , href (UB.relative [ "https://jisho.org/search", wordEntry.word ++ " #words" ] [])
        ]
        [ img
            [ src "images/ext-link.svg"
            , style "height" "12pt"
            , style "margin-left" "5pt"
            ]
            []
        ]


viewWordSelector : Model -> Html Msg
viewWordSelector model =
    if Array.isEmpty model.wordMatches then
        div [] []

    else
        div []
            [ text "Choose a word:"
            , ul [ style "list-style-type" "none" ]
                (List.map
                    (\( idx, wordEntry ) ->
                        div
                            []
                            [ li [ style "font-size" "medium" ]
                                [ input
                                    [ type_ "radio"
                                    , name "entrySelector"
                                    , value <| String.fromInt idx
                                    , onInput WordSelected
                                    , checked <| Just idx == model.selectedIndex
                                    ]
                                    []
                                , text wordEntry.word
                                ]
                            ]
                    )
                    (Array.toIndexedList model.wordMatches)
                )
            , button [ onClick SelectionConfirmed ] [ text "Confirm" ]
            ]


showInput : Model -> String
showInput model =
    case model.input.converted of
        Ok hiragana ->
            hiragana

        Err string ->
            string


wordEntryToString : WordEntry -> String
wordEntryToString wordEntry =
    wordEntry.word ++ " (" ++ wordEntry.kana ++ "): " ++ wordEntry.meaning


onEnter : msg -> Attribute msg
onEnter onEnterAction =
    on "keyup" <|
        Json.andThen
            (\keyCode ->
                if keyCode == 13 then
                    Json.succeed onEnterAction

                else
                    Json.fail (String.fromInt keyCode)
            )
            keyCode



-- HELPERS


{-| Remove the first occurrence of a value from a list.
-}
removeFromList : a -> List a -> List a
removeFromList x xs =
    case xs of
        [] ->
            []

        y :: ys ->
            if x == y then
                ys

            else
                y :: removeFromList x ys
