module Main exposing (..)

import Array exposing (..)
import Browser
import Browser.Dom as Dom
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (checked, disabled, href, id, name, placeholder, src, style, target, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Http
import Json.Decode as Json
import Random
import Task
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


{-| Show hint once timer if equal or less than this value
-}
hintTime : Int
hintTime =
    15



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


decrementTimer : Timer -> Timer
decrementTimer timer =
    { timer | value = timer.value - 1 }


resumeTimer : Timer -> Timer
resumeTimer timer =
    { timer | active = True }


pauseTimer : Timer -> Timer
pauseTimer timer =
    { timer | active = False }


resetTimer : Timer -> Timer
resetTimer timer =
    { timer | value = timer.maxValue }


type Message
    = GoodNews String
    | BadNews String


type alias Model =
    { kanjiToMatch : KanjiEntry
    , input : Input
    , wordMatches : List WordEntry
    , selectedIndex : Maybe Int
    , history : WordEntries
    , message : Maybe Message
    , kanjis : Dict Kanji KanjiEntry
    , candidateKanjis : List Kanji
    , unseenKanjis : List Kanji
    , jokerWord : Maybe WordEntry
    , hp : Int
    , timer : Timer
    , params : Params
    , score : Int
    , combo : Int
    , error : Maybe String
    }


initModel : Params -> Model
initModel params =
    { kanjiToMatch = defaultKanjiEntry
    , input = emptyInput
    , wordMatches = []
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
    , combo = 0
    , error = Nothing
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
    | GotJokerWord (Result Http.Error (Maybe WordEntry))
    | UpdatedInput String
    | GotConverted (Result Http.Error Input)
    | SubmittedInput
    | GotWordMatches (Result Http.Error WordEntries)
    | WordSelected WordEntry
    | Ticked Time.Posix
    | Focus (Result Dom.Error ())


withDebugLog : Msg -> Msg
withDebugLog message =
    case message of
        Ticked _ ->
            message

        _ ->
            Debug.log "Msg" message


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case model.error of
        Just error ->
            ( model, Cmd.none )

        Nothing ->
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

                        Err httpError ->
                            ( { model | error = Just ("Failed to retrieve kanjis: " ++ httpErrorToString httpError) }
                            , Cmd.none
                            )

                PickedKanji newKanji ->
                    ( updateKanjiToMatch model newKanji
                    , getJokerWord newKanji model.params.minJLPTLevel
                    )

                GotJokerWord result ->
                    case result of
                        Ok jokerWord ->
                            ( { model | jokerWord = Debug.log ("jokerWord for kanji " ++ model.kanjiToMatch.kanji) jokerWord }
                            , Cmd.none
                            )

                        Err httpError ->
                            ( { model | error = Just ("Failed to retrieve joker word: " ++ httpErrorToString httpError) }
                            , Cmd.none
                            )

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
                    ( updateInput model result
                    , Cmd.none
                    )

                SubmittedInput ->
                    -- Ugly trick to avoid the event after Enter has been pressed
                    -- to select a word
                    if model.timer.value > model.timer.maxValue - 1 then
                        ( model, Cmd.none )

                    else
                        case model.input.converted of
                            Ok hiragana ->
                                ( model
                                , searchWord hiragana model.kanjiToMatch.kanji
                                )

                            Err "" ->
                                case model.hp of
                                    1 ->
                                        ( { model | message = Just (BadNews "Can't give up with only 1 心 left !") }
                                        , Cmd.none
                                        )

                                    _ ->
                                        ( giveUp model, drawKanji model )

                            Err _ ->
                                -- Input is not valid, ignoring the submission
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
                                            ( { model | message = Just (BadNews "Already used this word before !") }
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
                                            ( { model | wordMatches = ws, timer = pauseTimer model.timer }
                                            , Cmd.none
                                            )

                        Err httpError ->
                            ( { model | error = Just ("Failed to find word matches: " ++ httpErrorToString httpError) }
                            , Cmd.none
                            )

                WordSelected wordEntry ->
                    let
                        newModel =
                            addWord model wordEntry
                    in
                    ( newModel, Cmd.batch [ drawKanji newModel, focusInputBox ] )

                Focus result ->
                    case result of
                        Err (Dom.NotFound notFoundMsg) ->
                            ( { model | error = Just ("DOM error when focusing on input box: " ++ notFoundMsg) }
                            , Cmd.none
                            )

                        Ok _ ->
                            ( model, Cmd.none )

                Ticked newTime ->
                    let
                        ( newModel, lostLife ) =
                            updateTimer model
                    in
                    ( newModel
                    , if lostLife then
                        drawKanji model

                      else
                        Cmd.none
                    )


focusInputBox : Cmd Msg
focusInputBox =
    Task.attempt Focus (Dom.focus "input-box")


filterNotAlreadySubmitted : Model -> List WordEntry -> List WordEntry
filterNotAlreadySubmitted model wordMatches =
    let
        notInHistory wordEntry =
            not (List.member wordEntry.word (List.map .word model.history))
    in
    List.filter notInHistory wordMatches


updateTimer : Model -> ( Model, Bool )
updateTimer model =
    if model.timer.value > 1 then
        ( { model | timer = decrementTimer model.timer }, False )

    else
        ( loseLife model, True )


loseLife : Model -> Model
loseLife model =
    let
        newHp =
            max (model.hp - 1) 0

        newTimer =
            resetTimer model.timer |> (\t -> { t | active = newHp > 0 })
    in
    { model
        | timer = newTimer
        , hp = newHp
        , combo = 0
        , input = emptyInput
        , jokerWord = Nothing
    }


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
        newMessage =
            case model.jokerWord of
                Just wordEntry ->
                    Just (BadNews ("Could have used " ++ wordEntryToString wordEntry))

                Nothing ->
                    Just (BadNews "Did not have any joker...")

        model_ =
            loseLife model
    in
    { model_
        | message = newMessage
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


updateInput : Model -> Result Http.Error Input -> Model
updateInput model result =
    case result of
        Ok newInput ->
            { model | input = newInput }

        Err httpError ->
            { model | error = Just ("Failed to convert input to hiragana: " ++ httpErrorToString httpError) }


noMatch : Model -> Model
noMatch model =
    loseLife model
        |> (\m ->
                { m
                    | message = Just (BadNews ("No match for " ++ showInput model ++ " !"))
                }
           )


addWord : Model -> WordEntry -> Model
addWord model wordEntry =
    let
        ( scoreIncr, comboIncr ) =
            scoreAndComboIncrease model wordEntry

        newTimer =
            resetTimer model.timer |> resumeTimer
    in
    { model
        | history = wordEntry :: model.history
        , wordMatches = []
        , selectedIndex = Nothing
        , input = emptyInput
        , jokerWord = Nothing
        , timer = newTimer
        , score = model.score + scoreIncr
        , combo = model.combo + comboIncr
        , message = Just (GoodNews ("+ " ++ String.fromInt scoreIncr ++ " points !"))
    }


scoreAndComboIncrease : Model -> WordEntry -> ( Int, Int )
scoreAndComboIncrease model wordEntry =
    let
        kanjiEntries =
            wordEntry.kanjis
                -- TODO deal properly with this
                |> List.map (\kanji -> Dict.get kanji model.kanjis |> Maybe.withDefault defaultKanjiEntry)

        kanjiScores =
            -- 5 pts for N1, 1 pts for N5
            List.map (\ke -> 6 - ke.jlpt) kanjiEntries

        baseScoreIncr =
            List.foldl (+) 0 kanjiScores

        scoreIncr =
            baseScoreIncr * (1 + model.combo)

        unseenKanjis =
            List.filter (\k -> List.member k.kanji model.unseenKanjis) kanjiEntries

        comboIncr =
            if List.isEmpty unseenKanjis then
                0

            else
                1
    in
    Debug.log "score and combo incr" ( scoreIncr, comboIncr )


romajiToHiragana : String -> Cmd Msg
romajiToHiragana romaji =
    case romaji of
        "" ->
            Cmd.none

        _ ->
            getJson
                (UB.relative [ apiBaseURL, "to-hiragana", romaji ] [])
                inputDecoder
                GotConverted


inputDecoder : Json.Decoder Input
inputDecoder =
    Json.map2 Input
        (Json.field "partial" Json.string)
        convertedDecoder


convertedDecoder : Json.Decoder (Result String Hiragana)
convertedDecoder =
    Json.field
        "valid"
        Json.bool
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


mainStyles : List (Attribute msg)
mainStyles =
    [ style "display" "flex"
    , style "flex-direction" "column"
    ]


globalMaxWidth : String
globalMaxWidth =
    "800px"


view : Model -> Html Msg
view model =
    case model.error of
        Just error ->
            viewError error

        Nothing ->
            let
                elems =
                    if model.hp > 0 then
                        [ viewInfos model
                        , viewKanjiToMatch model
                        , viewInput model
                        , viewMessageOrWordSelector model
                        , viewHistory model
                        ]

                    else
                        [ viewInfos model
                        , viewGameOver
                        , viewHistory model
                        ]
            in
            div [ style "max-width" globalMaxWidth, style "margin-left" "auto", style "margin-right" "auto" ] elems


viewGameOver : Html msg
viewGameOver =
    div [ style "margin-bottom" "30px" ]
        [ div [ style "font-size" "80px" ] [ text "Game over !" ]
        , div [ style "font-size" "2em", style "display" "flex", style "justify-content" "flex-start" ]
            [ div [ style "flex-grow" "0", style "margin" "0 50px" ] [ a [ href "" ] [ text "[Retry]" ] ]
            , div [ style "flex-grow" "0", style "margin" "0 50px" ] [ a [ href "." ] [ text "[Go back]" ] ]
            ]
        ]


viewError : String -> Html Msg
viewError error =
    div
        [ style "color" "red", style "max-width" globalMaxWidth, style "font-size" "x-large" ]
        [ text "ERROR !"
        , br [] []
        , br [] []
        , text error
        , br [] []
        , br [] []
        , div [ style "flex-grow" "0" ] [ a [ href "" ] [ text "[Retry]" ] ]
        , div [ style "flex-grow" "0" ] [ a [ href "." ] [ text "[Go back]" ] ]
        ]


mainDivStyles : List (Attribute msg)
mainDivStyles =
    [ style "padding" "5px"

    --, style "border" "1px #ccc solid"
    ]


infosStyles : List (Attribute msg)
infosStyles =
    [ style "display" "flex"
    , style "color" "white"
    , style "background-color" "black"
    , style "align-content" "center"
    ]


infosDivStyles : List (Attribute msg)
infosDivStyles =
    [ style "font-size" "large"
    ]


viewInfos : Model -> Html Msg
viewInfos model =
    let
        divFlexAndElems =
            [ ( "1"
              , [ text
                    ("心ｘ "
                        ++ String.padLeft 2 figureSpace (String.fromInt model.hp)
                    )
                ]
              )
            , ( "1"
              , [ text
                    ("タイマ： "
                        ++ String.padLeft 2 figureSpace (String.fromInt model.timer.value)
                    )
                ]
              )
            , ( "1"
              , [ text
                    ("点： "
                        ++ String.padLeft 5 figureSpace (String.fromInt model.score)
                    )
                ]
              )
            , ( "1"
              , [ text
                    ("コンボ： "
                        ++ String.padLeft 3 figureSpace (String.fromInt model.combo)
                    )
                ]
              )
            , ( "2"
              , [ text
                    ("漢字： "
                        ++ String.fromInt (List.length model.unseenKanjis)
                        ++ "／"
                        ++ String.fromInt (List.length model.candidateKanjis)
                        ++ " (Ｎ"
                        ++ String.fromInt model.params.minJLPTLevel
                        ++ ")"
                    )
                ]
              )
            ]

        subdivs =
            List.map (\( flex, elems ) -> div (infosDivStyles ++ [ style "flex" flex ]) elems) divFlexAndElems
    in
    div (mainDivStyles ++ infosStyles) subdivs


{-| Produces a space equal to the figure (0–9) characters.
-}
figureSpace : Char
figureSpace =
    '\u{2007}'


viewKanjiToMatch : Model -> Html Msg
viewKanjiToMatch model =
    div
        (mainDivStyles ++ [ style "display" "flex" ])
        [ viewKanji model.kanjiToMatch.kanji
        , viewKanjiInfos model
        ]


viewKanji : Kanji -> Html Msg
viewKanji kanji =
    div
        [ style "flex" "80px"
        , style "font-size" "80pt"
        , style "border" "1px #ccc solid"
        , style "text-align" "center"
        ]
        [ text kanji ]


viewKanjiInfos : Model -> Html Msg
viewKanjiInfos model =
    div
        [ style "flex" "8"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "stretch"
        , style "font-size" "medium"
        , style "margin" "0px 0px 0px 10px"
        ]
        [ viewKanjiMeaning model.kanjiToMatch
        , viewJLPTLevel model.kanjiToMatch
        , viewHint model
        ]


viewKanjiMeaning : KanjiEntry -> Html Msg
viewKanjiMeaning kanjiEntry =
    div
        [ style "flex" "1" ]
        [ text kanjiEntry.meaning
        ]


viewJLPTLevel : KanjiEntry -> Html Msg
viewJLPTLevel kanjiEntry =
    div
        [ style "flex" "1"
        , style "color" "gray"
        ]
        [ text (showJLPT kanjiEntry.jlpt) ]


viewHint : Model -> Html Msg
viewHint model =
    let
        elems =
            case ( model.jokerWord, model.timer.value <= hintTime ) of
                ( Just jokerWord, True ) ->
                    [ u [] [ text "Hint:" ]
                    , text (" " ++ jokerWord.meaning)
                    ]

                _ ->
                    []
    in
    div
        [ style "flex" "1"
        , style "font-size" "medium"
        , style "color" "blue"
        ]
        elems


showJLPT : Int -> String
showJLPT jlpt =
    "JLPT Ｎ" ++ String.fromInt jlpt


viewInput : Model -> Html Msg
viewInput model =
    div
        mainDivStyles
        [ input
            [ placeholder ("Type a new word with " ++ model.kanjiToMatch.kanji)
            , value model.input.romaji
            , onInput UpdatedInput
            , disabled (not (List.isEmpty model.wordMatches))
            , style "font-size" "xx-large"
            , id "input-box"
            , onEnter SubmittedInput
            ]
            []
        ]


viewMessageOrWordSelector : Model -> Html Msg
viewMessageOrWordSelector model =
    if List.isEmpty model.wordMatches then
        viewMessage model

    else
        viewWordSelector model


viewMessage : Model -> Html Msg
viewMessage model =
    let
        ( textValue, colorValue ) =
            case model.message of
                Just (GoodNews msg) ->
                    ( msg, "green" )

                Just (BadNews msg) ->
                    ( msg, "red" )

                Nothing ->
                    ( "", "black" )
    in
    div
        (mainDivStyles
            ++ [ style "font-size" "large"
               , style "min-height" "30pt"
               , style "color" colorValue
               ]
        )
        [ text textValue ]


viewWordSelector : Model -> Html Msg
viewWordSelector model =
    div
        (mainDivStyles ++ [ style "background-color" "yellow", style "font-size" "x-large" ])
        ([ text "Choose a word:"
         ]
            ++ List.map
                (\wordEntry ->
                    button
                        [ onClick (WordSelected wordEntry)
                        , style "font-size" "x-large"
                        , style "margin" "0 5px"
                        ]
                        [ text wordEntry.word ]
                )
                model.wordMatches
        )


viewHistory : Model -> Html Msg
viewHistory model =
    let
        historyElems =
            List.indexedMap Tuple.pair model.history
                |> List.map (\( idx, wordEntry ) -> viewWordEntry idx wordEntry)
    in
    div
        (mainDivStyles
            ++ [ style "display" "flex"
               , style "flex-direction" "column"
               ]
        )
        (text "Previous words:" :: historyElems)


viewWordEntry : Int -> WordEntry -> Html Msg
viewWordEntry idx wordEntry =
    div
        [ style "flex" "1"
        , style "display" "flex"
        , style "justify-content" "flex-start"
        , style "font-size"
            (if idx == 0 then
                -- Make latest entry bigger
                "x-large"

             else
                "medium"
            )
        ]
        [ div
            [ style "flex-shrink" "0" ]
            [ text (wordEntry.word ++ " (" ++ wordEntry.kana ++ "):") ]
        , div
            [ style "flex-shrink" "2"
            , style "flex-basis" "600px"
            , style "margin" "3px 0px 0px 10px"
            ]
            [ text wordEntry.meaning, viewWordDictLink wordEntry ]
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


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Http.Timeout ->
            "Unable to reach the server, try again"

        Http.NetworkError ->
            "Unable to reach the server, check your network connection"

        Http.BadStatus 500 ->
            "The server had a problem, try again later"

        Http.BadStatus 400 ->
            "Verify your information and try again"

        Http.BadStatus _ ->
            "Unknown error"

        Http.BadBody errorMessage ->
            errorMessage
