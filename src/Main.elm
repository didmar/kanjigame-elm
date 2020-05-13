module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (href, placeholder, src, style, target, value)
import Html.Events exposing (keyCode, on, onInput)
import Http
import Json.Decode as Json
import Random
import Time exposing (every)
import Url.Builder as Url



-- CONFIG


apiBaseURL : String
apiBaseURL =
    "http://127.0.0.1:9000"


maxKanjiGrade : Int
maxKanjiGrade =
    3


maxHP : Int
maxHP =
    10


maxTimer : Int
maxTimer =
    30



-- MAIN


main : Program () Model Msg
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
    , meaning : Maybe String
    , grade : Maybe Int
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
    { kanji = defaultKanji, meaning = Nothing, grade = Nothing }


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
    , wordMatches : WordEntries
    , history : WordEntries
    , message : Maybe String
    , kanjis : List Kanji
    , unseenKanjis : List Kanji
    , jokerWord : Maybe WordEntry
    , hp : Int
    , timer : Timer
    }


initModel : Model
initModel =
    { kanjiToMatch = defaultKanjiEntry
    , input = emptyInput
    , wordMatches = []
    , history = []
    , message = Nothing
    , kanjis = []
    , unseenKanjis = []
    , jokerWord = Nothing
    , hp = maxHP
    , timer = initTimer
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , getKanjis
    )



-- UPDATE


type Msg
    = GotKanjis (Result Http.Error (List Kanji))
    | PickedKanji Kanji
    | GotKanjiEntry (Result Http.Error KanjiEntry)
    | GotJokerWord (Result Http.Error (Maybe WordEntry))
    | UpdatedInput String
    | GotConverted (Result Http.Error (Result String Hiragana))
    | SubmittedInput
    | GotWordMatches (Result Http.Error WordEntries)
    | Ticked Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case Debug.log "Msg" message of
        GotKanjis result ->
            case result of
                Ok kanjis ->
                    let
                        newModel =
                            { model
                                | kanjis = kanjis
                                , unseenKanjis = kanjis
                            }
                    in
                    ( newModel
                    , drawKanji newModel
                    )

                Err _ ->
                    ( model, Cmd.none )

        PickedKanji newKanji ->
            ( updateKanjiToMatch model newKanji
            , Cmd.batch [ getJokerWord newKanji, getKanjiDetails newKanji ]
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

                        firstWordMatch :: otherWordMatches ->
                            let
                                newModel =
                                    addMatchedWord model firstWordMatch otherWordMatches
                            in
                            ( newModel
                            , drawKanji newModel
                            )

                Err _ ->
                    ( { model | input = emptyInput }
                    , Cmd.none
                    )

        Ticked newTime ->
            ( updateTimer model, Cmd.none )


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
        (Url.relative [ apiBaseURL, "kanji-details", kanji ] [])
        kanjiEntryDecoder
        GotKanjiEntry


kanjiEntryDecoder : Json.Decoder KanjiEntry
kanjiEntryDecoder =
    Json.map3 KanjiEntry
        (Json.field "kanji" Json.string)
        (Json.field "meaning" (Json.maybe Json.string))
        (Json.field "grade" (Json.maybe Json.int))


updateKanjiToMatch : Model -> Kanji -> Model
updateKanjiToMatch model newKanji =
    let
        newUnseenKanjis =
            removeFromList newKanji model.unseenKanjis
    in
    { model
        | kanjiToMatch = { kanji = newKanji, meaning = Nothing, grade = Nothing }
        , unseenKanjis =
            if List.isEmpty newUnseenKanjis then
                model.kanjis

            else
                newUnseenKanjis
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


getKanjis : Cmd Msg
getKanjis =
    getJson
        (Url.relative [ apiBaseURL, "kanjis" ] [ Url.int "max_grade" maxKanjiGrade ])
        kanjisDecoder
        GotKanjis


kanjisDecoder : Json.Decoder (List Kanji)
kanjisDecoder =
    Json.field "kanjis" (Json.list Json.string)


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
    drawKanjiFromList (Debug.log "draw from" kanjis)


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


getJokerWord : Kanji -> Cmd Msg
getJokerWord kanji =
    getJson
        (Url.relative
            [ apiBaseURL, "find-word-with-kanji", kanji ]
            [ Url.int "max_kanji_grade" maxKanjiGrade ]
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


addMatchedWord : Model -> WordEntry -> WordEntries -> Model
addMatchedWord model firstWordMatch otherWordMatches =
    { model
        | wordMatches = firstWordMatch :: otherWordMatches
        , history = firstWordMatch :: model.history
        , input = emptyInput
        , jokerWord = Nothing
    }


romajiToHiragana : String -> Cmd Msg
romajiToHiragana romaji =
    case romaji of
        "" ->
            Cmd.none

        _ ->
            getJson
                (Url.relative [ apiBaseURL, "to-hiragana", romaji ] [])
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
        (Url.relative
            [ apiBaseURL, "lookup-words", word ]
            [ Url.string "kanji_to_match" kanjiToMatch ]
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
                ++ " 漢字："
                ++ String.fromInt (List.length model.unseenKanjis)
                ++ "／"
                ++ String.fromInt (List.length model.kanjis)
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
        [ text (Maybe.withDefault "" kanjiEntry.meaning)
        , viewKanjiDictLink kanjiEntry.kanji
        ]


viewKanjiDictLink : Kanji -> Html Msg
viewKanjiDictLink kanji =
    a
        [ target "_blank"
        , href (Url.relative [ "https://jisho.org/search", kanji ++ " #kanji" ] [])
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


viewWordMatches : Model -> Html Msg
viewWordMatches model =
    div [ style "font-size" "medium" ] [ text (showWordMatches model) ]


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
        , href (Url.relative [ "https://jisho.org/search", wordEntry.word ++ " #words" ] [])
        ]
        [ img
            [ src "images/ext-link.svg"
            , style "height" "12pt"
            , style "margin-left" "5pt"
            ]
            []
        ]


showInput : Model -> String
showInput model =
    case model.input.converted of
        Ok hiragana ->
            hiragana

        Err string ->
            string


showWordMatches : Model -> String
showWordMatches model =
    case List.head model.wordMatches of
        Just wordMatch ->
            wordMatch.word

        _ ->
            ""


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
