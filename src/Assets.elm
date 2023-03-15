module Assets exposing (moth, lizard, bird, lover, grass, fart, sky, firefly, tree, sea, plant, flower, mothBody, frontWing, backWing)

import Helpers exposing (appendIf)
import Msg exposing (Msg)
import Model exposing (Model, ViewData, Flower(..))
import Html exposing (Attribute)
import Html.Attributes as Hats

{-| Moth -}
moth : ViewData -> List (Attribute Msg)
moth = 
    always 
        [ Hats.id "moth" 
        ]

mothBody : ViewData -> List (Attribute Msg)
mothBody =
    always 
        [ Hats.class "moth-body" 
        , Hats.class "idle-moth"
        ]

frontWing : ViewData -> List (Attribute Msg)
frontWing =
    always 
        [ Hats.class "moth-front-wing" 
        , Hats.class "idle-moth"
        ]

backWing : ViewData -> List (Attribute Msg)
backWing =
    always 
        [ Hats.class "moth-back-wing" 
        , Hats.class "idle-moth"
        ]

fart : Bool -> List (Attribute Msg)
fart first = 
    [ Hats.classList 
        [ ("fart", True)
        , ("fart-initial", first)
        , ("fart-stream", not first)
        ]
    ]

{-| NPCs -}
lizard : ViewData -> List (Attribute Msg)
lizard = always 
    [ Hats.id "lizard"
    ]

bird : ViewData -> List (Attribute Msg)
bird = always 
    [ Hats.id "bird" ]

lover : ViewData -> List (Attribute Msg)
lover = 
    always
        [ Hats.id "lover"
        , Hats.style "background-position" "-30px -18px"
        ]

{-| Scene -}
grass : a -> List (Attribute Msg)
grass grass_ = 
    let
        kind = [ Hats.class "grass-1"]
        {-case grass_.kind of
                NormalGrass ->
                    [ Hats.class "grass-1" ]
                DenseGrass ->
                    [ Hats.class "grass-2" ]
                TallGrass ->
                    [ Hats.class "grass-3" ]
                FloweringGrass ->
                    [ Hats.class "grass-4" ]
                CloveredGrass ->
                    [ Hats.class "grass-5" ]
                ShoreLineGrass ->
                    [ Hats.class "grass-6" ]-}
    in
    Hats.class "grass"
    :: kind
    |> appendIf True (Hats.class "grass-animated")
    --|> appendIf grass_.animated (Hats.class "grass-animated")


sky : ViewData -> List (Attribute Msg)
sky = 
    always 
    [ Hats.id "sky"

        ]

firefly : List (Attribute Msg)
firefly = []

tree : List (Attribute Msg)
tree = []

sea : ViewData -> List (Attribute Msg)
sea = always 
    [ Hats.id "sea" ]

plant : List (Attribute Msg)
plant = []

flower : Flower -> List (Attribute Msg)
flower flower_ =
    case flower_ of
        _ -> 
            [ Hats.class "flower-1"
            , Hats.class "flower"
            ]
