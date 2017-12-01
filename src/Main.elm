module Main exposing (main)

import Html exposing (Html, div, h1, h2, input, span, text, ul, li, program)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, value)
import Http
import Json.Decode as Decode
import Time exposing (Time)
import Date
import Task


type alias Id =
    Int


type Message
    = Click Id
    | ClickPerformed Id Time
    | AddButton
    | Logged (Result Http.Error String)
    | Focus Id
    | Index
    | Init (Result Http.Error String)
    | RenameButton Id String
    | ChangeButtonGroup Id String


type alias Button =
    { clicks : List Time, id : Id, text : String, group : String }


type alias Model =
    { uid : Id
    , buttons : List Button
    , focus : Maybe Id
    }


initialModel : Model
initialModel =
    { uid = 2, buttons = [ { clicks = [], id = 1, text = "Button", group = "" } ], focus = Nothing }


view : Model -> Html Message
view model =
    case model.focus of
        Just id ->
            let
                buttons =
                    List.filter (\b -> b.id == id) model.buttons

                groups =
                    List.filter (\b -> List.member b.group (List.map (\bg -> bg.group) buttons)) model.buttons
            in
                div [ class "container content" ] <|
                    List.map
                        (\button ->
                            div []
                                [ Html.button [ onClick Index, class "button" ] [ text "Back" ]
                                , h1 [ ] [ text button.text ]
                                , h2 [] [ text button.group ]
                                , span [ ] [ text "Name" ]
                                , input [ value button.text, class "input", onInput (RenameButton button.id) ] []
                                , span [ ] [ text "Group" ]
                                , input [ value button.group, class "input", onInput (ChangeButtonGroup button.id) ] []
                                , ul [] <| List.map (\click -> li [] [ text (toString (Date.fromTime click)) ]) button.clicks
                                ]
                        )
                        groups

        Nothing ->
            div [ class "container" ]
                [ div [ class "field is-grouped is-grouped-multiline" ] <| List.map viewButton model.buttons
                , Html.button [ onClick AddButton, class "button" ] [ text "New Button" ]
                ]


viewButton : Button -> Html Message
viewButton button =
    div [ class "control buttons has-addons" ]
        [ Html.button [ onClick (Click button.id), class "button" ] [ text (button.text ++ " (" ++ toString (List.length button.clicks) ++ ")") ]
        , Html.button [ onClick (Focus button.id), class "button is-danger" ] [ text "…" ]
        ]


updateButton : Id -> (Button -> Button) -> Button -> Button
updateButton id fn button =
    if id == button.id then
        fn button
    else
        button


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        Click id ->
            ( model, Task.perform (ClickPerformed id) Time.now )

        ClickPerformed id time ->
            ( clickButton model id time, log (ClickPerformed id time) )

        RenameButton id name ->
            ( renameButton model id name, log (RenameButton id name) )

        ChangeButtonGroup id group ->
            ( changeButtonGroup model id group, log (ChangeButtonGroup id group) )

        AddButton ->
            ( addButton model, log AddButton )

        Focus id ->
            ( { model | focus = Just id }, Cmd.none )

        Index ->
            ( { model | focus = Nothing }, Cmd.none )

        Logged _ ->
            ( model, Cmd.none )

        Init (Ok log) ->
            let
                m =
                    String.lines log
                        |> List.foldl (\l m -> initLine l m) model
            in
                ( m, Cmd.none )

        Init _ ->
            ( model, Cmd.none )


renameButton : Model -> Id -> String -> Model
renameButton model id newName =
    { model
        | buttons = List.map (updateButton id (\b -> { b | text = newName })) model.buttons
    }


changeButtonGroup : Model -> Id -> String -> Model
changeButtonGroup model id newGroup =
    { model
        | buttons = List.map (updateButton id (\b -> { b | group = newGroup })) model.buttons
    }


addButton : Model -> Model
addButton model =
    { model
        | uid = model.uid + 1
        , buttons = { clicks = [], id = model.uid, text = "Button " ++ (toString model.uid), group = "" } :: model.buttons
    }


clickButton : Model -> Id -> Time -> Model
clickButton model id time =
    { model
        | buttons = List.map (updateButton id (\b -> { b | clicks = (time :: b.clicks) })) model.buttons
    }


initLine : String -> Model -> Model
initLine line model =
    case String.words line of
        [ "AddButton" ] ->
            addButton model

        [ "ClickPerformed", sid, stime ] ->
            Result.withDefault model
                (Result.map2
                    (\id time ->
                        clickButton model id time
                    )
                    (String.toInt sid)
                    (String.toFloat stime)
                )

        [ "RenameButton", sid, name ] ->
            Result.withDefault model
                (Result.map
                    (\id ->
                        renameButton model id (String.dropRight 1 (String.dropLeft 1 name))
                    )
                    (String.toInt sid)
                )

        [ "ChangeButtonGroup", sid, group ] ->
            Result.withDefault model
                (Result.map
                    (\id ->
                        changeButtonGroup model id (String.dropRight 1 (String.dropLeft 1 group))
                    )
                    (String.toInt sid)
                )

        _ ->
            model


log : Message -> Cmd Message
log msg =
    Http.send Logged
        (Http.post "/log" (Http.stringBody "text/plain" (toString msg)) Decode.string)


load : Cmd Message
load =
    Http.send Init <| Http.getString "/log"


init : ( Model, Cmd Message )
init =
    ( initialModel, load )


main : Program Never Model Message
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }