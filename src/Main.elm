module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (placeholder, style, value)
import Html.Events exposing (keyCode, on, onInput)
import Http
import Json.Decode as Json
import Random



-- CONFIG


apiBaseURL : String
apiBaseURL =
    "http://127.0.0.1:9000/"


maxKanjiGrade : Int
maxKanjiGrade =
    3



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
    }


type alias WordEntries =
    List WordEntry


type alias Content =
    { romaji : String
    , converted : Result String Hiragana
    }


emptyContent : Content
emptyContent =
    { romaji = "", converted = Err "" }


type alias Model =
    { kanjiToMatch : KanjiEntry
    , content : Content
    , wordMatches : WordEntries
    , history : WordEntries
    , msg : Maybe String
    , kanjis : List Kanji
    , jokerWord : Maybe WordEntry
    }


initModel : Model
initModel =
    { kanjiToMatch = defaultKanjiEntry
    , content = emptyContent
    , wordMatches = []
    , history = []
    , msg = Nothing
    , kanjis = []
    , jokerWord = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , getKanjis
    )



-- UPDATE


type Msg
    = GotKanjis (Result Http.Error (List Kanji))
    | NewKanji Kanji
    | GotKanjiDetails (Result Http.Error KanjiEntry)
    | GotJokerWord (Result Http.Error (Maybe WordEntry))
    | InputRomaji String
    | GotConverted (Result Http.Error (Result String Hiragana))
    | Enter
    | GotWordMatches (Result Http.Error WordEntries)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Msg" msg of
        GotKanjis result ->
            case result of
                Ok kanjis ->
                    ( { model | kanjis = kanjis }
                    , drawKanji kanjis
                    )

                Err _ ->
                    ( model, Cmd.none )

        NewKanji newKanji ->
            ( updateKanjiToMatch model newKanji
            , Cmd.batch [ getJokerWord newKanji, getKanjiDetails newKanji ]
            )

        GotKanjiDetails result ->
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

        InputRomaji romaji ->
            case romaji of
                "" ->
                    ( { model | content = emptyContent, msg = Nothing }
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

        Enter ->
            case model.content.converted of
                Ok hiragana ->
                    ( model
                    , searchWord hiragana model.kanjiToMatch.kanji
                    )

                Err "" ->
                    ( giveUp model, drawKanji model.kanjis )

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
                            ( addMatchedWord model firstWordMatch otherWordMatches
                            , drawKanji model.kanjis
                            )

                Err _ ->
                    ( { model | content = emptyContent }
                    , Cmd.none
                    )


getKanjiDetails : Kanji -> Cmd Msg
getKanjiDetails kanji =
    getJson
        (apiBaseURL ++ "kanji-details/" ++ kanji)
        kanjiEntryDecoder
        GotKanjiDetails


kanjiEntryDecoder : Json.Decoder KanjiEntry
kanjiEntryDecoder =
    Json.map3 KanjiEntry
        (Json.field "kanji" Json.string)
        (Json.field "meaning" (Json.maybe Json.string))
        (Json.field "grade" (Json.maybe Json.int))


updateKanjiToMatch : Model -> Kanji -> Model
updateKanjiToMatch model newKanji =
    { model | kanjiToMatch = { kanji = newKanji, meaning = Nothing, grade = Nothing } }


updateRomaji : Model -> String -> Model
updateRomaji model romaji =
    let
        upd content =
            { content | romaji = romaji }
    in
    { model | content = upd model.content, msg = Nothing }


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
    { model | content = emptyContent, msg = Just newMsg }


getKanjis : Cmd Msg
getKanjis =
    getJson (apiBaseURL ++ "kanjis?max_grade=" ++ String.fromInt maxKanjiGrade)
        kanjisDecoder
        GotKanjis


kanjisDecoder : Json.Decoder (List Kanji)
kanjisDecoder =
    Json.field "kanjis" (Json.list Json.string)


drawKanji : List Kanji -> Cmd Msg
drawKanji kanjis =
    Random.generate NewKanji (kanjiGenerator kanjis)


getJokerWord : Kanji -> Cmd Msg
getJokerWord kanji =
    getJson (apiBaseURL ++ "find-word-with-kanji/" ++ kanji ++ "?max_kanji_grade=" ++ String.fromInt maxKanjiGrade)
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
                upd content =
                    { content | converted = converted }
            in
            { model | content = upd model.content }

        Err _ ->
            model


noMatch : Model -> Model
noMatch model =
    { model
        | content = emptyContent
        , msg = Just ("No match for " ++ showContent model ++ " !")
    }


addMatchedWord : Model -> WordEntry -> WordEntries -> Model
addMatchedWord model firstWordMatch otherWordMatches =
    { model
        | wordMatches = firstWordMatch :: otherWordMatches
        , history = firstWordMatch :: model.history
        , content = emptyContent
    }


romajiToHiragana : String -> Cmd Msg
romajiToHiragana romaji =
    case romaji of
        "" ->
            Cmd.none

        _ ->
            getJson
                (apiBaseURL ++ "to-hiragana/" ++ romaji)
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
        (apiBaseURL ++ "lookup-words/" ++ word ++ "?kanji_to_match=" ++ kanjiToMatch)
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
    Json.map3 WordEntry
        (Json.field "word" Json.string)
        kanaDecoder
        meaningDecoder


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
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ kanjiToMatchDiv model
        , wordInputDiv model
        , historyDiv model
        ]


kanjiToMatchDiv : Model -> Html Msg
kanjiToMatchDiv model =
    div [ style "min-height" "55pt" ]
        [ kanjiDiv model.kanjiToMatch.kanji
        , kanjiMeaningDiv model.kanjiToMatch.meaning
        ]


kanjiDiv : Kanji -> Html Msg
kanjiDiv kanji =
    div
        [ style "font-size" "xxx-large"
        , style "float" "left"
        , style "margin-right" "10pt"
        ]
        [ text (kanji ++ "?") ]


kanjiMeaningDiv : Maybe String -> Html Msg
kanjiMeaningDiv meaning =
    div
        [ style "font-size" "medium" ]
        [ text (Maybe.withDefault "" meaning) ]


wordInputDiv : Model -> Html Msg
wordInputDiv model =
    div
        []
        [ input
            [ placeholder ("Type a word with " ++ model.kanjiToMatch.kanji)
            , value model.content.romaji
            , onInput InputRomaji
            , onEnter Enter
            ]
            []
        , hiraganaOrMsgDiv model
        ]


hiraganaOrMsgDiv : Model -> Html Msg
hiraganaOrMsgDiv model =
    let
        ( color, txt ) =
            case model.msg of
                Just msg ->
                    ( "red", msg )

                Nothing ->
                    ( "black", showContent model )
    in
    div [ style "font-size" "medium", style "min-height" "18pt", style "color" color ] [ text txt ]


wordMatchesDiv : Model -> Html Msg
wordMatchesDiv model =
    div [ style "font-size" "medium" ] [ text (showWordMatches model) ]


historyDiv : Model -> Html Msg
historyDiv model =
    ul
        []
        (List.map wordEntryLi model.history)


wordEntryLi : WordEntry -> Html Msg
wordEntryLi wordEntry =
    li
        [ style "font-size" "medium", style "min-height" "18pt" ]
        [ text (wordEntryToString wordEntry) ]


showContent : Model -> String
showContent model =
    case model.content.converted of
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
