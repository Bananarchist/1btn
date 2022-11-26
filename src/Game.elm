module Game exposing (init, view, subscriptions, update)

import Audio
import Controls
import Html exposing (Html)
import Graphics
import Model exposing (Model)
import Painter
import Helpers exposing (flip, buildl, buildr, uncurry, duple)
import Map
import Moth
import NPCs
import Msg exposing (Msg)


init : { a | width : Int, height : Int, currentTick : Float } -> (Model, Cmd Msg)
init {width, height, currentTick} = 
    ( Model.init width height currentTick
    , Cmd.batch
        [ Msg.initializeAudio
        , Msg.generateGrass 100 
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

update : Msg -> Model -> (Model, Cmd Msg)
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

updateAnimations _ = identity

updateCurrentTick : Msg -> Model -> Model
updateCurrentTick msg model =
    case msg of
        Msg.Frame t ->
            Model.setCurrentTick t model
        _ -> model

updateLastTick : Msg -> Model -> Model
updateLastTick msg model =
    case msg of
        Msg.Frame t ->
            Model.setLastTick t model
        _ -> model

updateCmd : Msg -> Model -> (Model, Cmd Msg)
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
        ( buildr
            [ Painter.fieldView
            , Painter.playerView
            , Painter.lizardView
            , Painter.birdView
            , Painter.farFieldView
            , Painter.nearFieldView
            , Painter.loverView
            , Painter.mothView
            ]
        )
    >> uncurry Html.main_ 

