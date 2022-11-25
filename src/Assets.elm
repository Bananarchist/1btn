module Assets exposing (moth, lizard, bird, lover, grass, fart, sky, firefly, tree, sea, plant, flower)

import Helpers exposing (appendIf)
import Msg exposing (Msg)
import Model exposing (Model, Grass(..), Flower(..))
import Html exposing (Attribute)
import Html.Attributes as Hats

{-| Moth -}
moth : Model -> List (Attribute Msg)
moth = 
    always 
        [ Hats.id "moth" 
        , Hats.class "idle-moth"
        --, Hats.style "background-position" "0px 0px"
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
lizard : Model -> List (Attribute Msg)
lizard = always 
    [ Hats.id "lizard"
    ]

bird : Model -> List (Attribute Msg)
bird = always 
    [ Hats.id "bird" ]

lover : Model -> List (Attribute Msg)
lover = 
    always
        [ Hats.id "lover"
        , Hats.style "background-position" "-30px -18px"
        ]

{-| Scene -}
grass : { a | kind : Grass, animated : Bool } -> List (Attribute Msg)
grass grass_ = 
    let
        kind = 
            case grass_.kind of
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
                    [ Hats.class "grass-6" ]
    in
    Hats.class "grass"
    :: kind
    |> appendIf grass_.animated (Hats.class "grass-animated")


sky : Model -> List (Attribute Msg)
sky = 
    always 
    [ Hats.id "sky"

        ]

firefly : List (Attribute Msg)
firefly = []

tree : List (Attribute Msg)
tree = []

sea : Model -> List (Attribute Msg)
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
            , Hats.style "background-position" "-92px -124px"
            ]
