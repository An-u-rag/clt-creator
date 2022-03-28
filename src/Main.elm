module Main exposing (main)

{-
   Clt Creator application for cutting and transforming/translating a 3D clt plank.
   - 2 tabs
       1. Cutting
       2. Transform-Translate
   - Runs with elm-3d-scene

   Todo:
       1. Generate a 3D environment with a 3D clt plank.
       2. Add a background.
       3. Add orbiting camera on mouse click and drag.
       4. Look into 2D widgets for controls(sliders) and information.
-}
-- IMPORTS

import Angle exposing (Angle)
import Array
import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events
import Camera3d exposing (Camera3d, viewpoint)
import Color exposing (Color, black)
import Direction3d
import Duration exposing (Duration)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh exposing (Mesh)
import Svg
import Svg.Attributes as SA
import Task
import TriangularMesh exposing (TriangularMesh)
import TypingApp exposing (Msg(..))
import Viewpoint3d exposing (Viewpoint3d)
import WebGL.Texture exposing (Texture)



-- INIT


collageWidth =
    198


collageHeight =
    128


type WorldCoordinates
    = WorldCoordinates


type alias Window =
    { cw : Float
    , ch : Float
    , sw : Int
    , sh : Int
    }


type alias Model =
    { window : Window
    , elapsedTime : Duration
    , azimuth : Angle
    , elevation : Angle
    , isOrbiting : Bool
    , cltMesh1 : Mesh.Unlit WorldCoordinates
    , cltMesh2 : Mesh.Unlit WorldCoordinates
    , cltTopTexture : Material.Texture Color
    , cltSideTexture : Material.Texture Color
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        -- Combined mesh with upper + lower rectangular meshes and a Strip mesh for the sides (with variable widths and lengths)
        mesh =
            Mesh.texturedTriangles <|
                TriangularMesh.combine [ upperMesh, lowerMesh ]

        upperMesh =
            TriangularMesh.indexed
                (Array.fromList
                    [ { position = Point3d.centimeters 0 0 1, uv = ( 0.0, 0.0 ) } -- 0
                    , { position = Point3d.centimeters 4 0 1, uv = ( 1.0, 0.0 ) } -- 1
                    , { position = Point3d.centimeters 4 3 1, uv = ( 1.0, 1.0 ) } -- 2
                    , { position = Point3d.centimeters 0 3 1, uv = ( 0.0, 1.0 ) } -- 3
                    , { position = Point3d.centimeters 0 0 1, uv = ( 0.0, 0.0 ) }
                    ]
                )
                [ ( 0, 1, 2 )
                , ( 2, 3, 0 )
                ]

        lowerMesh =
            TriangularMesh.indexed
                (Array.fromList
                    [ { position = Point3d.centimeters 0 0 0, uv = ( 0.0, 0.0 ) } -- 0
                    , { position = Point3d.centimeters 4 0 0, uv = ( 1.0, 0.0 ) } -- 1
                    , { position = Point3d.centimeters 4 3 0, uv = ( 1.0, 1.0 ) } -- 2
                    , { position = Point3d.centimeters 0 3 0, uv = ( 0.0, 1.0 ) } -- 3
                    , { position = Point3d.centimeters 0 0 0, uv = ( 0.0, 0.0 ) }
                    ]
                )
                [ ( 0, 1, 2 )
                , ( 2, 3, 0 )
                ]

        rawStripMesh =
            Mesh.texturedTriangles <|
                TriangularMesh.strip
                    [ { position = Point3d.centimeters 0 0 1, uv = ( 0.0, 0.0 ) } -- 0
                    , { position = Point3d.centimeters 4 0 1, uv = ( 1.0, 0.0 ) } -- 1
                    , { position = Point3d.centimeters 4 3 1, uv = ( 0.0, 0.0 ) } -- 2
                    , { position = Point3d.centimeters 0 3 1, uv = ( 1.0, 0.0 ) } -- 3
                    , { position = Point3d.centimeters 0 0 1, uv = ( 0.0, 0.0 ) }
                    ]
                    [ { position = Point3d.centimeters 0 0 0, uv = ( 0.0, 1.0 ) } -- 0
                    , { position = Point3d.centimeters 4 0 0, uv = ( 1.0, 1.0 ) } -- 1
                    , { position = Point3d.centimeters 4 3 0, uv = ( 0.0, 1.0 ) } -- 2
                    , { position = Point3d.centimeters 0 3 0, uv = ( 1.0, 1.0 ) } -- 3
                    , { position = Point3d.centimeters 0 0 0, uv = ( 0.0, 1.0 ) }
                    ]
    in
    ( { window = { cw = collageWidth, ch = collageHeight, sw = 0, sh = 0 }
      , elapsedTime = Quantity.zero
      , azimuth = Angle.degrees 45
      , elevation = Angle.degrees 30
      , isOrbiting = False
      , cltMesh1 = mesh
      , cltMesh2 = rawStripMesh
      , cltTopTexture = Material.constant Color.black
      , cltSideTexture = Material.constant Color.black
      }
    , Cmd.batch
        [ getViewportSize
        , Task.attempt (GotTexture "top") (Material.loadWith Material.trilinearFiltering cltTopTextureURL)
        , Task.attempt (GotTexture "side") (Material.loadWith Material.trilinearFiltering cltSideTextureURL)
        ]
    )


cltTopTextureURL : String
cltTopTextureURL =
    "https://raw.githubusercontent.com/An-u-rag/elm-3d-clt-playground/main/clt-textures/clt1.png"


cltSideTextureURL : String
cltSideTextureURL =
    "https://raw.githubusercontent.com/An-u-rag/elm-3d-clt-playground/main/clt-textures/crosssection-2.png"



-- Update


type Msg
    = Tick Duration
    | WindowResize (Maybe ( Int, Int ))
    | MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | GotTexture String (Result WebGL.Texture.Error (Material.Texture Color))
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick duration ->
            ( { model | elapsedTime = Quantity.plus duration model.elapsedTime }, Cmd.none )

        WindowResize mWH ->
            case mWH of
                Just ( w, h ) ->
                    ( { model
                        | window = didResize model.window w h
                      }
                    , Cmd.none
                    )

                -- need to get viewport size after the app starts
                Nothing ->
                    ( model
                    , getViewportSize
                    )

        MouseDown ->
            ( { model | isOrbiting = True }, Cmd.none )

        MouseUp ->
            ( { model | isOrbiting = False }, Cmd.none )

        MouseMove dx dy ->
            if model.isOrbiting then
                let
                    -- How fast we want to orbit the camera (orbiting the
                    -- camera by 1 degree per pixel of drag is a decent default
                    -- to start with)
                    rotationRate =
                        Angle.degrees 1 |> Quantity.per Pixels.pixel

                    -- Adjust azimuth based on horizontal mouse motion (one
                    -- degree per pixel)
                    newAzimuth =
                        model.azimuth
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    -- Adjust elevation based on vertical mouse motion (one
                    -- degree per pixel), and clamp to make sure camera cannot
                    -- go past vertical in either direction
                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                in
                ( { model | azimuth = newAzimuth, elevation = newElevation }, Cmd.none )

            else
                ( model, Cmd.none )

        GotTexture textureType (Ok texture) ->
            if textureType == "top" then
                -- Successfully loaded the texture
                ( { model | cltTopTexture = texture }, Cmd.none )

            else
                ( { model | cltSideTexture = texture }, Cmd.none )

        GotTexture textureType (Err error) ->
            if textureType == "top" then
                -- Network error, bad image dimensions etc.
                ( { model | cltTopTexture = Material.constant Color.blue }, Cmd.none )

            else
                ( { model | cltSideTexture = Material.constant Color.blue }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isOrbiting then
        -- If we're currently orbiting, listen for mouse moves and mouse button
        -- up events (to stop orbiting); in a real app we'd probably also want
        -- to listen for page visibility changes to stop orbiting if the user
        -- switches to a different tab or something
        Sub.batch
            [ Browser.Events.onMouseMove decodeMouseMove
            , Browser.Events.onMouseUp (Decode.succeed MouseUp)
            , Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)
            ]

    else
        -- If we're not currently orbiting, just listen for mouse down events
        -- to start orbiting
        Sub.batch
            [ Browser.Events.onMouseDown (Decode.succeed MouseDown)
            , Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)
            ]



-- view


didResize : Window -> Int -> Int -> Window
didResize window sw sh =
    { window | sw = round <| toFloat sw * 0.99, sh = round <| toFloat sh * 0.95 }


getViewportSize : Cmd Msg
getViewportSize =
    Task.attempt
        (\rvp ->
            case rvp of
                Ok vp ->
                    WindowResize <|
                        Just ( floor <| vp.viewport.width, floor <| vp.viewport.height )

                Err _ ->
                    NoOp
        )
        getViewport


view : Model -> Browser.Document Msg
view model =
    let
        -- Create a fixed directional light
        sunlight =
            Light.directional (Light.castsShadows False)
                { chromaticity = Light.daylight
                , intensity = Illuminance.lux 10000
                , direction = Direction3d.yz (Angle.degrees -120)
                }

        -- Create some soft overhead lighting the same color as the background,
        -- so that the block appears to be lit by its surroundings
        overheadLighting =
            Light.overhead
                { upDirection = Direction3d.positiveZ
                , chromaticity = Light.daylight
                , intensity = Illuminance.lux 10000
                }

        -- Function to generate a CLT Plank with variable width, length and height
        viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.meters 0 0 0
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters 0.5
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    { title = "CLTCreator"
    , body =
        [ div []
            [ h1 [ style "margin" "0px", style "text-align" "center" ] [ text "CLTCreator" ]
            , Scene3d.custom
                { camera = camera
                , clipDepth = Length.centimeters 0.5
                , dimensions = ( Pixels.int model.window.sw, Pixels.int model.window.sh )
                , antialiasing = Scene3d.multisampling
                , lights = Scene3d.twoLights sunlight overheadLighting
                , exposure = Scene3d.exposureValue 12
                , toneMapping = Scene3d.noToneMapping
                , whiteBalance = Light.daylight
                , background = Scene3d.backgroundColor Color.grey
                , entities =
                    [ Scene3d.mesh (Material.texturedColor model.cltTopTexture) model.cltMesh1
                    , Scene3d.mesh (Material.texturedColor model.cltSideTexture) model.cltMesh2
                    ]
                }
            ]
        ]
    }



-- Main


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
