module Main exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes as Attr
import Maybe exposing (withDefault)
import Task
import Time exposing (millisToPosix, toHour, toMinute)


type StyleMode
    = Dark
    | Light


type alias Model =
    { time : Time.Posix
    , zone : Time.Zone
    , sessions : List Session
    , styleMode : StyleMode
    }


type alias SessionWithTime =
    { start : Time.Posix
    , end : Time.Posix
    , session : Session
    }


type alias Session =
    { time : String
    , link : String
    , title : String
    , speaker : String
    , room : String
    }


init : ( Model, Cmd Msg )
init =
    ( { time = millisToPosix 0
      , zone = Time.utc
      , sessions =
            [ Session "9:30 AM - 10:10 AM" "/2023/new-algorithms-for-collaborative-text-editing.html" "New algorithms for collaborative text editing" "Martin Kleppmann" "US Grand F"
            , Session "9:30 AM - 10:10 AM" "/2023/experimentation-putting-research-papers-into-prod.html" "Experimentation: putting research papers into prod" "Leemay Nassery" "US Grand DE"
            , Session "9:30 AM - 10:10 AM" "/2023/using-data-driven-metrics-to-anticipate-and-prevent-security-incidents.html" "Using data-driven metrics to anticipate and prevent security incidents" "Caitlin Buckshaw" "US Grand ABC"
            , Session "9:30 AM - 10:10 AM" "/2023/playable-quotes-for-game-boy-games.html" "Playable Quotes for Game Boy Games" "Joël Franušić, Adam Smith" "US Regency AB"
            , Session "10:20 AM - 11:00 AM" "/2023/the-economics-of-programming-languages.html" "The Economics of Programming Languages" "Evan Czaplicki" "US Grand F"
            , Session "10:20 AM - 11:00 AM" "/2023/war-time-proofs-and-futuristic-programs.html" "War Time Proofs and Futuristic Programs" "Valeria de Paiva" "US Grand DE"
            , Session "10:20 AM - 11:00 AM" "/2023/computational-physics-beyond-the-glass.html" "Computational Physics, Beyond the Glass" "Sam Ritchie" "US Grand ABC"
            , Session "10:20 AM - 11:00 AM" "/2023/didnt-chrome-already-have-a-root-store.html" "Didn't Chrome Already Have a Root Store?" "David Adrian" "US Regency AB"
            , Session "10:20 AM - 11:00 AM" "/2023/an-approach-to-computing-and-sustainability-inspired-from-permaculture.html" "An approach to computing and sustainability inspired from permaculture" "Devine Lu Linvega" "US Regency C"
            , Session "11:00 AM - 01:00 PM" "/2023/friday-lunch.html" "Friday Lunch" "Lunch!" "Lunch!"
            , Session "11:20 AM - 12:00 PM" "/2023/can-a-programming-language-reason-about-systems.html" "Can a Programming Language Reason About Systems?" "Marianne Bellotti" "US Grand F"
            , Session "11:25 AM - 12:05 PM" "/2023/from-geometry-to-algebra-and-back-again-4000-years-of-papers.html" "From Geometry to Algebra and Back Again: 4000 Years of Papers" "Jack Rusher" "US Grand DE"
            , Session "11:30 AM - 12:10 PM" "2023/cursorless-a-spoken-language-for-editing-code.html" "Cursorless: A Spoken Language for Editing Code" "Pokey Rule" "US Grand ABC"
            , Session "12:00 PM - 12:40 PM" "/2023/unmasking-the-godfather---reverse-engineering-the-latest-android-banking-trojan.html" "Unmasking the Godfather - Reverse Engineering the Latest Android Banking Trojan" "Laurie Kirk" "US Regency AB"
            , Session "12:05 PM - 12:45 PM" "/2023/birdsong-as-code.html" "Birdsong as code" "Chris Ford" "US Regency C"
            , Session "1:40 PM - 2:20 PM" "/2023/a-long-strange-loop.html" "A Long Strange Loop" "Alex Miller" "Stifel Theatre"
            , Session "2:30 PM - 3:15 PM" "/2023/how-to-make-hard-things-easy.html" "How to Make Hard Things Easy" "Julia Evans" "Stifel Theatre"
            , Session "3:30 PM - 4:15 PM" "/2023/drawing-comics-at-work.html" "Drawing Comics at Work" "Randall Munroe" "Stifel Theatre"
            , Session "4:15 PM - 4:45 PM" "/2023/closing.html" "Closing" "Alex Miller" "Stifel Theatre"
            , Session "5:00 PM - 6:30 PM" "/2023/closing-reception-and-signing.html" "Closing Reception and Signing" "Randall Munroe" "Stifel Theatre"
            ]
      , styleMode = Dark
      }
    , Task.perform AdjustTimeZone Time.here
    )


sessionToSessionWithTime : Time.Zone -> Time.Posix -> Session -> SessionWithTime
sessionToSessionWithTime zone now session =
    let
        start =
            stringToTime zone now <| String.left 8 session.time

        end =
            stringToTime zone now <| String.right 8 session.time
    in
    { start = start, end = end, session = session }


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )

        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ Attr.class <| "container " ++ toString model.styleMode ]
        [ h1 [] [ text "Strangeloop 2023 Friday Schedule" ]
        , div [ Attr.class "CardView" ] <| sessionsWithTimeview model <| List.map (sessionToSessionWithTime model.zone model.time) model.sessions
        , div [ Attr.class "ListView" ] <| currentView model.sessions
        ]


sessionsWithTimeview : Model -> List SessionWithTime -> List (Html msg)
sessionsWithTimeview model sessions =
    List.map
        (\sessionsInGroup ->
            let
                timeS =
                    String.append "time-" <| String.replace ":" "" <| String.replace "-" " time-" <| String.replace " " "" <| withDefault "0" <| Maybe.andThen (\x -> Just x.session.time) <| List.head sessionsInGroup
            in
            div [ Attr.class <| "group " ++ timeS ] <|
                List.map (sessionView model) sessionsInGroup
        )
    <|
        groupedByTime sessions


groupedByTime : List SessionWithTime -> List (List SessionWithTime)
groupedByTime sessions =
    List.map
        (\session ->
            List.filter
                (\s ->
                    s.session.time == session.session.time
                )
                sessions
        )
        sessions


stringToTime : Time.Zone -> Time.Posix -> String -> Time.Posix
stringToTime zone now time =
    let
        hour =
            withDefault 0 <| String.toInt <| String.left 2 time

        minute =
            withDefault 0 <| String.toInt <| String.right 2 time

        t_hour =
            toHour zone now

        t_min =
            toMinute zone now

        millseconds =
            (hour - t_hour) * 60 * 60 * 1000 + (minute - t_min) * 60 * 1000
    in
    millisToPosix millseconds


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init =
            \_ -> init
        , update = update
        , subscriptions = \_ -> Time.every 1000 Tick
        }


sessionsHappeningNow : Model -> List SessionWithTime -> List SessionWithTime
sessionsHappeningNow model sessions =
    List.filter
        (\session ->
            let
                hourZone =
                    Time.toHour model.zone
            in
            hourZone session.start
                <= hourZone model.time
                && hourZone model.time
                <= hourZone session.end
        )
        sessions


timeUntil : Time.Zone -> Time.Posix -> Time.Posix -> String
timeUntil zone now time =
    let
        now_s =
            Time.toSecond zone now

        now_m =
            Time.toMinute zone now

        now_h =
            Time.toHour zone now

        time_s =
            Time.toSecond zone time

        time_m =
            Time.toMinute zone time

        time_h =
            Time.toHour zone time
    in
    if [ time_h, time_m, time_s ] > [ now_h, now_m, now_s ] then
        let
            seconds =
                time_s - now_s

            minutes =
                time_m - now_m

            hours =
                time_h - now_h
        in
        String.join ":"
            [ String.fromInt hours
            , String.fromInt minutes
            , String.fromInt seconds
            ]

    else if [ time_h, time_m, time_s ] < [ now_h, now_m, now_s ] then
        let
            minutes =
                time_m - now_m

            hours =
                now_h - time_h
        in
        String.join ":"
            [ String.fromInt hours
            , String.fromInt minutes
            ]

    else
        "now"


sessionView : Model -> SessionWithTime -> Html msg
sessionView model session =
    div [ Attr.class "session box gradient-border" ]
        [ h3 [ Attr.class "title" ] [ a [ Attr.href <| "https://thestrangeloop.com" ++ session.session.link ] [ text session.session.title ] ]
        , p [ Attr.class "speaker" ] [ text session.session.speaker ]
        , p [ Attr.class "time" ] [ text session.session.time ]
        , p [ Attr.class "location" ] [ text session.session.room ]
        ]


currentView : List Session -> List (Html msg)
currentView sessions =
    List.map
        (\session ->
            div []
                [ h3 [] [ a [ Attr.href <| "https://thestrangeloop.com" ++ session.link ] [ text session.title ] ]
                , p [] [ text session.speaker ]
                , p [] [ text session.time ]
                ]
        )
        sessions
