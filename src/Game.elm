module Game exposing (init, subscriptions, update, view)

import Audio
import Controls
import Graphics
import Helpers exposing (buildl, buildr, duple, flip, uncurry)
import Html exposing (Html)
import Map
import Model exposing (Model, viewData)
import Moth
import Msg exposing (Msg)
import NPCs
import Painter
import Pixels
import Duration


init : { a | width : Int, height : Int, currentTick : Float } -> ( Model, Cmd Msg )
init { width, height, currentTick } =
    ( Model.init width height currentTick
    , Cmd.batch
        [ Msg.initializeAudio
        --, Msg.generateGrass 100
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions =
    buildl
        [ Controls.controllerSubs
        , always [ Msg.visibilitySub, Msg.resizeSub ]
        , Graphics.graphicsSubs
        ]
        >> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updateModel msg model
        |> updateCmd msg


updateModel : Msg -> Model -> Model
updateModel msg model =
    List.foldl
        ((|>) msg)
        model
        [ updateCurrentTick
        , Map.updateMap
        , Controls.updateControls
        , Moth.updateMoth
        , NPCs.updateNPCs
        , updateAnimations
        , updateLastTick
        ]


updateAnimations _ =
    identity


updateCurrentTick : Msg -> Model -> Model
updateCurrentTick msg model =
    case msg of
        Msg.Frame t ->
            Model.setCurrentTick t model

        _ ->
            model


updateLastTick : Msg -> Model -> Model
updateLastTick msg model =
    case msg of
        Msg.Frame t ->
            Model.setLastTick t model

        _ ->
            model


updateCmd : Msg -> Model -> ( Model, Cmd Msg )
updateCmd msg model =
    List.foldr
        ((|>) msg >> (|>) model >> (++))
        [ Cmd.none ]
        [ Moth.updateMothCmds
        , Audio.updateAudioCmds
        , Controls.updateControlCmds
        , Graphics.updateGraphicCmds
        , Map.updateMapCmds
        ]
        |> Cmd.batch
        |> Tuple.pair model


view : Model -> Html Msg
view =
    duple
        >> Tuple.mapFirst (always [])
        >> Tuple.mapSecond
            (viewData
                >> buildr
                [ Painter.fieldView
                , Painter.playerView
                , Painter.lizardView
                , Painter.birdView
                , Painter.farFieldView
                , Painter.loverView
                , Painter.mothView
                , Painter.nearFieldView
                ]
            )
        >> uncurry Html.main_
