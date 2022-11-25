module Painter exposing (..)

import Assets
import Graphics
import Helpers exposing (appendIf, buildl, buildr, flip)
import Html exposing (Html)
import Html.Attributes as Hats
import Model exposing (Model)
import Msg exposing (Msg)


div : List (Html.Attribute Msg) -> Html Msg
div =
    flip Html.div []


fieldView : Model -> List (Html Msg)
fieldView model =
    [ Html.div [ Hats.id "field" ] []
    ]


skyView : Model -> List (Html Msg)
skyView =
    Assets.sky
        >> div
        >> List.singleton


seaView : Model -> List (Html Msg)
seaView =
    buildl
        [ Assets.sea
        , Graphics.transform
            >> Graphics.translate Model.seaCoords
            >> Graphics.attributes
        ]
        >> div
        >> List.singleton


playerView : Model -> List (Html Msg)
playerView model =
    gasView model
        ++ notificationView model


notificationView : Model -> List (Html Msg)
notificationView =
    Model.notification
        >> Maybe.map
            (Html.text
                >> List.singleton
                >> Html.div [ Hats.class "notification" ]
                >> List.singleton
            )
        >> Maybe.withDefault []


gasView : Model -> List (Html Msg)
gasView =
    Model.gas
        >> String.fromFloat
        >> Hats.value
        >> List.singleton
        >> List.append
            [ Hats.min "0"
            , Hats.max "100"
            , Hats.attribute "low" "35"
            , Hats.attribute "high" "70"
            , Hats.attribute "optimum" "90"
            ]
        >> flip Html.meter []
        >> List.singleton


lizardView : Model -> List (Html Msg)
lizardView =
    buildl
        [ Assets.lizard
        , Graphics.transform
            >> Graphics.translate Model.lizardCoords
            >> Graphics.attributes
        ]
        >> div
        >> List.singleton


birdView : Model -> List (Html Msg)
birdView =
    buildl
        [ Assets.bird
        , Graphics.transform
            >> Graphics.translate Model.birdCoords
            >> Graphics.attributes
        ]
        >> div
        >> List.singleton


farFieldView : Model -> List (Html Msg)
farFieldView =
    buildr
        [ treeView
        , plantView
        ]


treeView : Model -> List (Html Msg)
treeView =
    always []



{-
   Model.trees
       >> List.map
           (buildl
               [ always Assets.tree
               , .pos >> Graphics.translation
               , .scale >> Graphics.scale
               ]
               >> div
           )
-}


plantView : Model -> List (Html Msg)
plantView =
    Model.flowers
        >> List.map
            (buildl
                [ .kind >> Assets.flower
                , Graphics.transform
                    >> Graphics.translate .coords
                    >> Graphics.attributes
                ]
                >> div
            )


nearFieldView : Model -> List (Html Msg)
nearFieldView =
    buildl
        [ seaView
        , grassView
        , fireflyView
        ]


grassView : Model -> List (Html Msg)
grassView =
    Model.visibleGrassSegments
        >> List.map
            (buildl
                [ Assets.grass
                , Graphics.transform
                    >> Graphics.translate .coords
                    >> Graphics.mirrorX .mirrored
                    >> Graphics.attributes
                ]
                >> div
            )
        >> Html.div [ Hats.id "grasses" ]
        >> List.singleton


fireflyView : Model -> List (Html Msg)
fireflyView =
    always []



{- Model.fireflies
   >> List.map
       (buildl
           [ always Assets.firefly
           , Graphics.translation
           , .scale >> Graphics.scale
           ]
           >> div
       )
-}


loverView : Model -> List (Html Msg)
loverView =
    buildl
        [ Assets.lover
        , Graphics.transform
            >> Graphics.translate Model.loverCoords
            >> Graphics.attributes
        ]
        >> div
        >> List.singleton


mothView : Model -> List (Html Msg)
mothView =
    buildl
        [ Assets.moth
        , Graphics.transform
            >> Graphics.translate Model.mothCoords
            >> Graphics.rotate Model.mothRot
            >> Graphics.mirrorX Model.mothMirroredか
            >> Graphics.attributes
        ]
        >> div
        >> List.singleton


fartView : Model -> List (Html Msg)
fartView =
    Model.farts
        >> List.concatMap
            -- for each fart stream
            (List.indexedMap
                -- for each element of the fart stream
                (\idx ->
                    buildl
                        [ always (Assets.fart (idx == 0))
                        , Graphics.transform
                            >> Graphics.translate .coords
                            >> Graphics.rotate .rotation
                            >> Graphics.attributes
                        ]
                        >> div
                )
            )
        >> Html.div [ Hats.id "farts" ]
        >> List.singleton



{-
   Model.farts
       >> List.map
           (buildl
               [ always Assets.fart
               , .pos >> Graphics.translation
               , .scale >> Graphics.scale
               ]
               >> div
           )
-}
