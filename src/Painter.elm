module Painter exposing (..)

import Assets
import Graphics
import Helpers exposing (appendIf, buildl, buildr, flip, duple, uncurry)
import Html exposing (Html)
import Html.Attributes as Hats
import Model exposing (EntityViewData, Model, ViewData)
import Moth
import Msg exposing (Msg)


div : List (Html.Attribute Msg) -> Html Msg
div =
    flip Html.div []


fieldView : ViewData -> List (Html Msg)
fieldView model =
    [ Html.div [ Hats.id "field" ] []
    ]


skyView : ViewData -> List (Html Msg)
skyView =
    Assets.sky
        >> div
        >> List.singleton


seaView : ViewData -> List (Html Msg)
seaView =
    {- buildl
       [ Assets.sea
       , Graphics.transform
           >> Graphics.translate Model.seaCoords
           >> Graphics.attributes
       ]
       >> div
       >> List.singleton
    -}
    always []


playerView : ViewData -> List (Html Msg)
playerView model =
    gasView model
        ++ notificationView model


notificationView : ViewData -> List (Html Msg)
notificationView =
    always Nothing
        --Model.notification
        >> Maybe.map
            (Html.text
                >> List.singleton
                >> Html.div [ Hats.class "notification" ]
                >> List.singleton
            )
        >> Maybe.withDefault []


gasView : ViewData -> List (Html Msg)
gasView =
    .gas
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


lizardView : ViewData -> List (Html Msg)
lizardView =
    buildl
        [ Assets.lizard

        --, Graphics.transform
        -->> Graphics.translate Model.lizardSP
        -->> Graphics.attributes
        ]
        >> div
        >> List.singleton


birdView : ViewData -> List (Html Msg)
birdView =
    buildl
        [ Assets.bird

        --, Graphics.transform
        --   >> Graphics.translate Model.birdSP
        --  >> Graphics.attributes
        ]
        >> div
        >> List.singleton


farFieldView : ViewData -> List (Html Msg)
farFieldView =
    buildr
        [ treeView
        , plantView
        ]


treeView : ViewData -> List (Html Msg)
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


plantView : ViewData -> List (Html Msg)
plantView =
       .flowers
           >> List.map
               (buildl
                   [ .kind >> Assets.flower
                   , Graphics.transform
                       >> Graphics.translate .coordinates
                       >> Graphics.attributes
                   ]
                   >> div
               )


nearFieldView : ViewData -> List (Html Msg)
nearFieldView =
    buildl
        [ seaView
        , grassView
        , fireflyView
        ]


grassView : ViewData -> List (Html Msg)
grassView =
    .grass >>
        List.map
            (buildl
                [ Assets.grass
                , Graphics.transform
                    >> Graphics.translate .coordinates
                    -->> Graphics.translate .coords
                    -->> Graphics.mirrorX .mirrored
                    >> Graphics.attributes
                ]
                >> div
            )
        >> Html.div [ Hats.id "grasses" ]
        >> List.singleton


fireflyView : ViewData -> List (Html Msg)
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


mothTransformations : (ViewData -> EntityViewData) -> ViewData -> List (Html.Attribute Msg)
mothTransformations accessor =
    Graphics.transform
        >> Graphics.translate (accessor >> .position)
        >> Graphics.rotate (accessor >> .rotation)
        >> Graphics.mirrorX (accessor >> .mirrorX)
        >> Graphics.attributes

        {-
mothAssembly : (ViewData -> EntityViewData) -> { betweenBackAndBody : List (ViewData -> List (Html Msg)), betweenBodyAndFront : List (ViewData -> List (Html Msg)) } -> List (ViewData -> List (Html Msg))
mothAssembly accessor { betweenBackAndBody, betweenBodyAndFront } =
    (buildl [ Assets.backWing, mothTransformations accessor ] >> div >> List.singleton)
        :: betweenBackAndBody
        ++ (buildl [ Assets.body, mothTransformations accessor ] >> div >> List.singleton)
        :: betweenBodyAndFront
        ++ [ buildl [ Assets.frontWing, mothTransformations accessor ] >> div >> List.singleton ]
        -}


loverView : ViewData -> List (Html Msg)
loverView =
    mothParts (buildl [ Assets.lover, mothTransformations .lover ])


mothView : ViewData -> List (Html Msg)
mothView =
    mothParts (buildl [ Assets.moth, mothTransformations .moth ])

mothParts : (ViewData -> List (Html.Attribute Msg)) -> ViewData -> List (Html Msg)
mothParts enclosingAttrFn =
    duple
    >> Tuple.mapBoth enclosingAttrFn 
        (buildl 
            [ Assets.backWing >> div >> List.singleton
            , fartView
            , Assets.mothBody >> div >> List.singleton
            , Assets.frontWing >> div >> List.singleton
            ])
    >> uncurry Html.div
    >> List.singleton

fartView : ViewData -> List (Html Msg)
fartView =
    always []
        --Model.farts
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
