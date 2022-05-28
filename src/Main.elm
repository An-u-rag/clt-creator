module Main exposing (main)

{-
   Clt Creator application for cutting and transforming/translating a 3D clt plank.
   - 2 tabs
       1. Cutting
       2. Transform-Translate
   - Runs with elm-3d-scene

   Todo:
       1. Generate a 3D environment with a 3D clt plank. (Done)
       2. Add a background. (Done)
       3. Add orbiting camera on mouse click and drag. (Done)
       4. Overlay - Look into 2D widgets for controls(sliders) and information.
       5. Add dynamic resizing of the scene. (Done)
       6. XY plane Grid (done)
       7. Make CLT plank Clickable -> Highlight and Focus with onClick.
-}
-- IMPORTS

import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d
import Block3d exposing (axes)
import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events
import Camera3d exposing (Camera3d, perspective, viewpoint)
import CltPlank exposing (..)
import Color exposing (Color, black, blue, darkGreen, lightOrange)
import Cylinder3d exposing (Cylinder3d)
import Dict
import Direction3d
import Duration exposing (Duration)
import Frame3d exposing (Frame3d)
import GraphicSVG exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra exposing (andMap)
import Length exposing (Meters)
import Pixels exposing (Pixels, float)
import Point3d exposing (Point3d, coordinates)
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh exposing (Mesh)
import Svg
import Svg.Attributes as SA
import Task
import TriangularMesh exposing (TriangularMesh, vertex)
import Viewpoint3d exposing (Viewpoint3d)
import WebGL.Texture exposing (Texture)
import Wrapper3D
import Skybox exposing (..)


-- INIT
-- Collage dimensions will be used to generate various overlays later using Scalable vector graphics.


collageWidth =
    270


collageHeight =
    128


type ProjectionType
    = Perspective
    | Orthographic
    | Isometric


type AnimationState
    = Off
    | Ready
    | Camera
    | Cutting


slider : Char -> Float -> Float -> Float -> Html Msg
slider axis position min halfMax =
    div []
        [ Html.text (String.fromChar axis)
        , input
            [ type_ "range"
            , Html.Attributes.min (String.fromFloat min)
            , Html.Attributes.max <|
                String.fromFloat (halfMax * 2)
            , value (String.fromFloat position)
            , onInput (UpdateSawBladeSlider axis)
            ]
            []
        , Html.text (String.fromFloat position)
        ]


cutterUI model =
    group
        [ group
            [ textBox 0 0 False False [ "2D" ] |> move ( 0, 0 )
            ]
        , group
            (if model.numCuts == 2 then
                [ textBox 8 8 (model.selectedId == 0) False [ "I" ] |> move ( -55, -15 ) |> notifyTap (SelectPlank 0)
                , textBox 8 8 (model.selectedId == 1) False [ "II" ] |> move ( 50, -15 ) |> notifyTap (SelectPlank 1)
                , textBox 8 8 (model.selectedId == 2) False [ "III" ] |> move ( 50, 10 ) |> notifyTap (SelectPlank 2)
                , textBox 8 8 (model.selectedId == 3) False [ "IV" ] |> move ( -55, 10 ) |> notifyTap (SelectPlank 3)
                ]

             else if model.numCuts == 1 && model.cutDir == 'X' then
                [ textBox 8 8 (model.selectedId == 1) False [ "I" ] |> move ( -55, 0 ) |> notifyTap (SelectPlank 1)
                , textBox 8 8 (model.selectedId == 0) False [ "II" ] |> move ( 55, 0 ) |> notifyTap (SelectPlank 0)
                ]

             else if model.numCuts == 1 && model.cutDir == 'Y' then
                [ textBox 8 8 (model.selectedId == 0) False [ "I" ] |> move ( 0, -15 ) |> notifyTap (SelectPlank 0)
                , textBox 8 8 (model.selectedId == 1) False [ "II" ] |> move ( 0, 15 ) |> notifyTap (SelectPlank 1)
                ]

             else
                [ textBox 0 0 (model.selectedId == 0) False [ "" ] |> move ( 15, 15 )
                ]
            )
        ]



-- Shapes model to render 2D overlay using GraphicSVG


myShapes model =
    [ textBox2 35 60 False False [] |> move ( 100, 20 ) |> makeTransparent 0.8 --main, biggest box
    , pipelines |> move ( 130, -15 )
    , textBox 20
        5
        False
        False
        (case model.projection of
            Orthographic ->
                [ "Orthographic" ]

            Perspective ->
                [ "Perspective" ]

            Isometric ->
                [ "Isometric" ]
        )
        |> move ( 97, 55 )
        |> notifyTap ViewToggle
    , textBox 30 5 False False [ "OPERATIONS" ] |> move ( 100, 45 )
    , html 200 20 (slider 'X' model.sawBladeTop.x 0 ((*) 100 <| .x <| Point3d.toMeters model.cltMain.centerPoint)) |> scale 0.2 |> move ( 83, -15 ) |> notifyEnter (BlockOrbiting True) |> notifyLeave (BlockOrbiting False)
    , html 200 20 (slider 'Y' model.sawBladeLeft.y 0 ((*) 100 <| .y <| Point3d.toMeters model.cltMain.centerPoint)) |> scale 0.2 |> move ( 83, -25 ) |> notifyEnter (BlockOrbiting True) |> notifyLeave (BlockOrbiting False)
    , textBox 30 5 False False [ "Rotate along X axis" ] |> move ( 100, 36 ) |> notifyTap (RotateObject model.selectedId 'X')
    , textBox 30 5 False False [ "Rotate along Y axis" ] |> move ( 100, 30 ) |> notifyTap (RotateObject model.selectedId 'Y')
    , textBox 30 5 False False [ "Rotate along Z axis" ] |> move ( 100, 24 ) |> notifyTap (RotateObject model.selectedId 'Z')
    , textBox 30 5 False False [ "Cutter" ] |> move ( 100, 12 ) |> notifyTap Set2D
    , textBox 10 5 False False [ "Cut" ] |> move ( 90, 0 ) |> notifyTap (Cut 2 ' ')
    , textBox 10 5 False False [ "CutX" ] |> move ( 100, 0 ) |> notifyTap (Cut 1 'X')
    , textBox 10 5 False False [ "CutY" ] |> move ( 110, 0 ) |> notifyTap (Cut 1 'Y')
    , textBox 30 5 False False [ "Focus" ] |> move ( 100, 6 ) |> notifyTap (FocusChange model.cltMain.centerPoint)
    , textBox 30 5 False False [ "Reset" ] |> move ( 100, -6 ) |> notifyTap Reset

    --  , textBox 30 5 False False [ "Play" ] |> move ( -80, 60 ) |> notifyTap AnimationToggle
    , animationButton model |> move ( 112.5, 56 ) |> notifyTap AnimationToggle
    , GraphicSVG.text "CLT" |> fixedwidth |> bold |> GraphicSVG.size 9 |> filled GraphicSVG.darkBlue |> move ( -130, 55 )
    , GraphicSVG.text "Creator" |> fixedwidth |> bold |> GraphicSVG.size 4 |> filled GraphicSVG.darkBlue |> move ( -130, 50 )
    , yourCode model
    , if model.elevation == Angle.degrees 90 && model.focusAt /= Point3d.meters 0 0 0 then
        cutterUI model

      else
        textBox 0 0 True False [ "" ] |> move ( 0, 0 )
    ]


yourCode model =
    group
        [ rect 40 60
            |> filled white
            |> makeTransparent 0.8
            |> addOutline (solid 0.3) darkBlue
            |> move ( -110, -20 )
        , if model.isCut then
            codeGen model.cltList

          else
            codeGen [ model.cltMain ]
        ]


codeGen : List CltPlank -> Shape userMsg
codeGen cltList =
    group <|
        (if List.length cltList == 1 then
            List.map2 (\y clt -> clt |> move ( 0, -y )) [ 0 ]

         else
            List.map2 (\y clt -> clt |> move ( 0, -y )) [ 0, 12, 24, 36 ]
        )
        <|
            List.map
                (\clt ->
                    group
                        [ GraphicSVG.text (plankMeshCode clt)
                            |> GraphicSVG.size 2.5
                            |> customFont "monospace"
                            |> filled darkBlue
                            |> move ( -128, 6 )
                        , GraphicSVG.text (plankRot "X" clt)
                            |> GraphicSVG.size 2.5
                            |> customFont "monospace"
                            |> filled darkBlue
                            |> move ( -122, 3 )
                        , GraphicSVG.text (plankRot "Y" clt)
                            |> GraphicSVG.size 2.5
                            |> customFont "monospace"
                            |> filled darkBlue
                            |> move ( -122, 0 )
                        , GraphicSVG.text (plankRot "Z" clt)
                            |> GraphicSVG.size 2.5
                            |> customFont "monospace"
                            |> filled darkBlue
                            |> move ( -122, -3 )
                        ]
                )
                cltList


plankMeshCode clt =
    "CltPlank "
        ++ Debug.toString clt.width
        ++ " "
        ++ Debug.toString clt.length
        ++ " "
        ++ Debug.toString clt.height


plankRot dir clt =
    case dir of
        "X" ->
            if clt.rotationAngleX == Angle.degrees 0 then
                ""

            else
                "|> RotX "
                    ++ Debug.toString (Angle.inDegrees clt.rotationAngleX)

        "Y" ->
            if clt.rotationAngleY == Angle.degrees 0 then
                ""

            else
                "|> RotY "
                    ++ Debug.toString (Angle.inDegrees clt.rotationAngleY)

        "Z" ->
            if clt.rotationAngleZ == Angle.degrees 0 then
                ""

            else
                "|> RotZ "
                    ++ Debug.toString (Angle.inDegrees clt.rotationAngleZ)

        _ ->
            ""



--animationButton


animationButton model =
    case model.animationState of
        Off ->
            playButton 3

        Ready ->
            playPlusCamButton

        Camera ->
            pauseButton 1.5 5

        _ ->
            pauseButton 1.5 5



-- Play button


playButton side =
    [ group
        [ rect 4 4
            |> filled darkBlue
            |> makeTransparent 0.1
        , triangle side
            |> filled darkBlue
        ]
    ]
        |> group
        |> scale 1.2
        |> move ( -1.5, -1.25 )



-- Pause button


pauseButton width height =
    [ group
        [ rect 4 5
            |> filled darkBlue
            |> makeTransparent 0.1
        , rect width height
            |> filled darkBlue
        , rect width height
            |> filled darkBlue
            |> move ( -2.5, 0 )
        ]
        |> scale 1.15
        |> move ( 0, -1.25 )
    ]
        |> group



--play + cam button


playPlusCamButton =
    [ group
        [ rect 24 24
            |> filled darkBlue
            |> makeTransparent 0.1
        , rect 12 6
            |> filled darkBlue
            |> subtract (circle 2 |> ghost |> move ( 0, 0 ))
            |> subtract (circle 0.5 |> ghost |> move ( 2, 2 ))
            |> move ( 5, 5 )
        , rect 2 3
            |> filled darkBlue
            |> move ( 9, 7 )
        ]
    , triangle 12
        |> filled darkBlue
        |> move ( -5, -4 )
    ]
        |> group
        |> scale 0.3



-- pipelines


pipelines =
    [ group
        [ roundedRect 83 3 1
            --rightmost
            |> filled darkBlue
            |> rotate (degrees 90)
        , roundedRect 240 3 1
            --longest
            |> filled darkBlue
            |> move ( -120, -40 )
        , roundedRect 7 3 1
            --code linker
            |> filled darkBlue
            |> rotate (degrees 90)
            |> move ( -240, -38 )
        , roundedRect 12 3 1
            --operations linker
            |> filled darkBlue
            |> move ( -6, 40 )
        ]
    ]
        |> group



-- Textbox overlay template


textBox width height isHighlighted isSelectable chars =
    [ rect width height
        |> filled
            (if isHighlighted then
                green

             else
                white
            )
        |> makeTransparent 0.8
    , if isSelectable then
        GraphicSVG.text (String.join "" <| List.reverse chars)
            |> centered
            |> customFont "monospace"
            |> GraphicSVG.size 2.5
            -- |> selectable
            |> filled GraphicSVG.darkBlue
            |> clip (rect width height |> ghost)

      else
        GraphicSVG.text (String.join "" <| List.reverse chars)
            |> centered
            |> customFont "monospace"
            |> GraphicSVG.size 2.5
            |> filled GraphicSVG.darkBlue
            |> clip (rect width height |> ghost)
    , rect width height
        |> outlined (solid 0.3) darkBlue
    ]
        |> group



-- blueprint box


textBox2 width height isHighlighted isSelectable chars =
    [ rect width height |> filled darkBlue |> makeTransparent 0.8
    , if isSelectable then
        GraphicSVG.text (String.join "" <| List.reverse chars)
            |> centered
            |> GraphicSVG.size 3
            -- |> selectable
            |> filled GraphicSVG.darkBlue
            |> clip (rect width height |> ghost)

      else
        GraphicSVG.text (String.join "" <| List.reverse chars)
            |> centered
            |> GraphicSVG.size 3
            |> filled GraphicSVG.darkBlue
            |> clip (rect width height |> ghost)
    , rect width height
        |> outlined (solid 1.5)
            (if isHighlighted then
                darkBlue

             else
                darkBlue
            )
    ]
        |> group



-- To create Sawblade teeth


spokes : Float -> Float -> Wrapper3D.Object WorldCoordinates
spokes counter angle =
    if counter == 0 then
        Wrapper3D.group3D []

    else
        Wrapper3D.group3D
            [ Wrapper3D.polyCylinder [ ( -19.81, 35.207 ), ( -8.694, 38.628 ), ( -11.54, 39.198 ), ( -12.97, 40.623 ), ( -12.68, 46.04 ), ( -19.81, 41.763 ), ( -19.81, 35.207 ) ] 4 { generatedMeshes = Dict.empty, generatedShadows = Dict.empty }
                |> Wrapper3D.metallic Color.darkGray 0.1
                |> Wrapper3D.rotateY3D (degrees 90)
                |> Wrapper3D.rotateX3D (degrees (angle * counter))
                |> Wrapper3D.move3D ( 0, 0, 25 )
            , spokes (counter - 1) angle
            ]



-- MouseWheel data for zooming


type alias MouseWheelEvent =
    { deltaX : Float
    , deltaY : Float
    , clientX : Float
    , clientY : Float
    }



-- Window is a type alias for a custom record used to hold values for Collage and Scene Dimensions.


type alias Window =
    { cw : Float
    , ch : Float
    , sw : Int
    , sh : Int
    }


type alias SawBladeData =
    { x : Float
    , y : Float
    , z : Float
    }



-- Main Model (State variable) type which is used to store current state values for the application.


type alias Model =
    { window : Window
    , projection : ProjectionType
    , time : Float
    , animationState : AnimationState
    , rotationAngle : Angle
    , azimuth : Angle
    , elevation : Angle
    , zoom : Float
    , focusAt : Point3d Meters WorldCoordinates
    , selectedId : Int
    , isOrbiting : Bool
    , isOrbitBlock : Bool
    , sawBladeTop : SawBladeData
    , sawBladeLeft : SawBladeData
    , isCut : Bool
    , cutDir : Char
    , numCuts : Int
    , cltMain : CltPlank
    , cltList : List CltPlank
    , gridTexture : Material.Texture Color.Color
    , skyboxTexture : Material.Texture Color.Color
--    , skybox : Scene3d.Entity WorldCoordinates
    }



-- Initialization of the model state variable as well as the app on initial load.
-- The initialization contains the mesh information for the CLT slab which holds the vector adn uv coordinates in Point3d centimeters and float respectively.


init : () -> ( Model, Cmd Msg )
init () =
    let
        width =
            365

        length =
            1825

        height =
            30

        indexedMesh =
            meshV width length height

        stripMesh =
            stripMeshV width length height

        centerPoint =
            calcCenter (upperMeshV width length height) (rawStripMeshV width length height)
    in
    -- In the init functio nwe store the previously created mesh and other values since creation of a mesh is an expensive operation
    --   and changing the mesh frequently causes optimization issues.
    -- This is why we store the mesh in the state change variable instead of calculating and remaking the mesh at every instance.
    -- We also store the texture values for the created meshes to apply them later in the view function
    -- time is a data type used to handle time w.r.t Animation and frames.
    -- azimuth and elevation hold the values required to position the camera. These are dynamically changed while the user drags on the screen.
    -- isOrbiting is a boolean which holds true if the user wants to drag the screen else it is false.
    -- cltMeshx and clt_Texture hold mesh and texture values as discussed above.
    -- Coming to the commands, we are issuing 3 separate commands on initialisation.
    -- getViewportSize is used to get the current viewport dimensions of the browser from javascript.
    -- The other two commands are used to make a request to the github repositories which hold the textures.
    ( { window = { cw = collageWidth, ch = collageHeight, sw = 0, sh = 0 }
      , projection = Perspective
      , time = 0
      , animationState = Camera
      , rotationAngle = Quantity.zero
      , azimuth = Angle.degrees 45
      , elevation = Angle.degrees 30
      , zoom = 4000
      , focusAt = Point3d.centimeters 0 0 0
      , selectedId = -1
      , isOrbiting = False
      , isOrbitBlock = False
      , sawBladeTop =
            { x = .x <| Point3d.toRecord Length.inCentimeters centerPoint
            , y = (+) 300 <| (*) 2 <| .y <| Point3d.toRecord Length.inCentimeters centerPoint
            , z = .z <| Point3d.toRecord Length.inCentimeters centerPoint
            }
      , sawBladeLeft =
            { x = (-) 1500 <| (*) 2 <| .x <| Point3d.toRecord Length.inCentimeters centerPoint
            , y = .y <| Point3d.toRecord Length.inCentimeters centerPoint
            , z = .z <| Point3d.toRecord Length.inCentimeters centerPoint
            }
      , isCut = False
      , cutDir = ' '
      , numCuts = 0
      , cltMain =
            { width = width
            , length = length
            , height = height
            , offsetX = 0
            , offsetY = 0
            , rotationAngleX = Angle.degrees 0
            , rotationAngleY = Angle.degrees 0
            , rotationAngleZ = Angle.degrees 0
            , centerPoint = centerPoint
            , cltFrame = Frame3d.atPoint centerPoint
            , indexedMesh = indexedMesh
            , stripMesh = stripMesh
            , cltTopTexture = Material.constant Color.black
            , cltSideTexture = Material.constant Color.black
            }
      , cltList = []
      , gridTexture = Material.constant Color.black
      , skyboxTexture = Material.constant Color.brown
      }
    , Cmd.batch
        [ getViewportSize

        -- Important to note that we render the below textures using trilinearFiltering texture/ image filtering.
        , Task.attempt (GotTexture "top") (Material.loadWith Material.bilinearFiltering cltTopTextureURL)
        , Task.attempt (GotTexture "side") (Material.loadWith Material.trilinearFiltering cltSideTextureURL)
        , Task.attempt (GotTexture "grid") (Material.loadWith Material.trilinearFiltering gridTextureURL)
        , Task.attempt (GotTexture "skybox") (Material.loadWith Material.trilinearFiltering skyboxTextureURL)
        ]
    )



-- URLs for the Grid


gridTextureURL : String
gridTextureURL =
    "https://raw.githubusercontent.com/An-u-rag/elm-3d-clt-playground/main/GraphPaperTextures/GraphPaper4096-aligned.png"


skyboxTextureURL : String
skyboxTextureURL =
    "https://raw.githubusercontent.com/An-u-rag/elm-3d-clt-playground/main/warehouseTexture.png"

-- UPDATE
-- This datatype acts like a messgae/action handler for changing state (or updating) the current state of our app.
-- The various things it can represent is separated with a "|" (pipe) symbol.
-- We have state changes for :
--  Tick - time based animation frames
--  WindowResize - To update the values of viewport size in the model.
--  Mouse Events - Mouse down and Up for controling camera and Mouse move to get the location of drag points.
--  GotTexture - For updating the texure in the model after retrieval.
--  NoOp - Like a default state change where model/ state does not change.


type Msg
    = Tick Duration
    | ViewToggle
    | WindowResize (Maybe ( Int, Int ))
    | ReturnPosition (( Float, Float ) -> Msg) ( Float, Float )
    | MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | BlockOrbiting Bool
    | GotTexture String (Result WebGL.Texture.Error (Material.Texture Color.Color))
    | FocusChange (Point3d Meters WorldCoordinates)
    | RotateObject Int Char
    | AnimationToggle
    | Cut Int Char
    | Set2D
    | SelectPlank Int
    | CheckZoom
    | Zoom MouseWheelEvent
    | UpdateSawBladeSlider Char String
    | Reset
    | NoOp



-- The actual update function which has case by case for each of the state change initiators from "Msg" datatype above.
-- THis function dicatates WHAT to change, the actual action in the state change, and HOW to change it.


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Tick is used to state change for every animation frame.
        Tick duration ->
            let
                updatedTime =
                    (+) model.time <| Duration.inSeconds duration
            in
            ( { model
                | time = updatedTime
              }
            , Cmd.none
            )

        ViewToggle ->
            case model.projection of
                Perspective ->
                    ( { model | projection = Orthographic }, Cmd.none )

                Orthographic ->
                    ( { model | projection = Isometric }, Cmd.none )

                Isometric ->
                    ( { model | projection = Perspective }, Cmd.none )

        AnimationToggle ->
            case model.animationState of
                Off ->
                    ( { model | animationState = Ready, time = 0 }, Cmd.none )

                Ready ->
                    ( { model | animationState = Camera }, Cmd.none )

                Camera ->
                    ( { model | animationState = Off }, Cmd.none )

                _ ->
                    ( { model | animationState = Off }, Cmd.none )

        -- if model.isAnimating then
        --     ( { model | isAnimating = False }, Cmd.none )
        -- else
        --     ( { model | isAnimating = True }, Cmd.none )
        -- Rotation object: Sawblade
        -- Rotation axis: For top sawblade-> about its own axis (parallel to X axis)
        --                For left sawblade-> about its own axis (parallel to Y axis)
        -- Rotation angle: 360 degrees per second (1 full rotation will be complete in 1 second)
        -- Window Resize is setting the viewport dimensions in the model when there is a successful retrieval from Javascrip and elm runtime.
        WindowResize mWH ->
            case mWH of
                Just ( w, h ) ->
                    ( { model
                        | window = didResize model.window w h
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model
                    , getViewportSize
                    )

        ReturnPosition message ( x, y ) ->
            let
                ( newModel, userCmds ) =
                    update
                        (message (convertCoords model.window ( x, y )))
                        model
            in
            ( newModel, userCmds )

        -- Mouse events which are used to set the boolean value for isOrbiting in the model based on whetehr the user is clicking and dragging.
        MouseDown ->
            if model.isOrbitBlock then
                ( { model | isOrbiting = False }, Cmd.none )

            else
                ( { model | isOrbiting = True }, Cmd.none )

        MouseUp ->
            ( { model | isOrbiting = False }, Cmd.none )

        -- If the user isOrbiting then we get and set the new azimuth and elevation values into the model for renderign in the view.
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
                ( { model
                    | azimuth = newAzimuth
                    , elevation = newElevation
                    , animationState =
                        if model.animationState /= Cutting && model.animationState == Camera then
                            Ready

                        else
                            model.animationState
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        BlockOrbiting b ->
            ( { model | isOrbitBlock = b }, Cmd.none )

        -- Setting the retrieved texture values into the model.
        GotTexture textureType (Ok texture) ->
            if textureType == "top" then
                -- Successfully loaded the texture
                ( { model | cltMain = updateCltTexture model.cltMain "TopTexture" texture }, Cmd.none )

            else if textureType == "grid" then
                ( { model | gridTexture = texture }, Cmd.none )

            else if textureType == "skybox" then
                ( { model | skyboxTexture = texture }, Cmd.none )

            else
                ( { model | cltMain = updateCltTexture model.cltMain "SideTexture" texture }, Cmd.none )

        GotTexture textureType (Err error) ->
            if textureType == "top" then
                ( { model | cltMain = updateCltTexture model.cltMain "TopTexture" (Material.constant Color.blue) }, Cmd.none )

            else if textureType == "grid" then
                ( { model | gridTexture = Material.constant Color.blue }, Cmd.none )

            else
                ( { model | cltMain = updateCltTexture model.cltMain "SideTexture" (Material.constant Color.blue) }, Cmd.none )

        FocusChange point ->
            ( { model
                | focusAt =
                    point
              }
            , Cmd.none
            )

        Cut numCuts cutDir ->
            ( if model.isCut then
                model

              else
                { model
                    | isCut = True
                    , animationState = Cutting
                    , numCuts = numCuts
                    , cutDir = cutDir
                    , cltList = updateCltList model.cltList numCuts cutDir model model.cltMain
                    , time = 0
                }
            , Cmd.none
            )

        RotateObject id axis ->
            let
                selectablePlank =
                    if id == -1 then
                        model.cltMain

                    else
                        Maybe.withDefault defaultPlank <| Array.get id <| Array.fromList model.cltList

                {- cltList =
                   if id == -1 then
                       model.cltList
                   else
                       selectablePlank
                -}
            in
            if id == -1 then
                ( { model | cltMain = rotateClt selectablePlank axis }, Cmd.none )

            else
                ( { model | cltList = rotateCltInList model.cltList selectablePlank axis }, Cmd.none )

        Set2D ->
            ( { model | azimuth = Angle.degrees 270, elevation = Angle.degrees 90 }, Cmd.none )

        SelectPlank id ->
            let
                selectablePlank =
                    Maybe.withDefault defaultPlank <| Array.get id <| Array.fromList model.cltList
            in
            ( { model | selectedId = id, focusAt = selectablePlank.centerPoint }, Cmd.none )

        CheckZoom ->
            ( model, Cmd.none )

        Zoom e ->
            if e.deltaY > 0 && model.zoom < 6000 then
                ( { model | zoom = model.zoom + 200 }, Cmd.none )

            else if e.deltaY < 0 && model.zoom > 2000 then
                ( { model | zoom = model.zoom - 200 }, Cmd.none )

            else
                ( model, Cmd.none )

        UpdateSawBladeSlider axis pos ->
            case String.toFloat pos of
                Just p ->
                    if axis == 'X' then
                        ( { model | sawBladeTop = updatePos model.sawBladeTop p axis }, Cmd.none )

                    else
                        ( { model | sawBladeLeft = updatePos model.sawBladeLeft p axis }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Reset ->
            ( { model
                | isCut = False
                , numCuts = 0
                , cutDir = ' '
                , cltMain = resetPlank model.cltMain
                , rotationAngle = Quantity.zero
                , focusAt = Point3d.centimeters 0 0 0
                , selectedId = -1
                , sawBladeTop =
                    { x = .x <| Point3d.toRecord Length.inCentimeters model.cltMain.centerPoint
                    , y = (+) 300 <| (*) 2 <| .y <| Point3d.toRecord Length.inCentimeters model.cltMain.centerPoint
                    , z = .z <| Point3d.toRecord Length.inCentimeters model.cltMain.centerPoint
                    }
                , sawBladeLeft =
                    { x = (-) 1500 <| (*) 2 <| .x <| Point3d.toRecord Length.inCentimeters model.cltMain.centerPoint
                    , y = .y <| Point3d.toRecord Length.inCentimeters model.cltMain.centerPoint
                    , z = .z <| Point3d.toRecord Length.inCentimeters model.cltMain.centerPoint
                    }
                , cltList = []
              }
            , Cmd.none
            )

        -- Default catch to make no change to model/state.
        NoOp ->
            ( model, Cmd.none )


rotateCltInList : List CltPlank -> CltPlank -> Char -> List CltPlank
rotateCltInList cltList clt dir =
    List.map
        (\c ->
            if c == clt then
                rotateClt c dir

            else
                c
        )
        cltList


updatePos : SawBladeData -> Float -> Char -> SawBladeData
updatePos sb pos axis =
    if axis == 'X' then
        { sb | x = pos }

    else
        { sb | y = pos }


updateCltList : List CltPlank -> Int -> Char -> Model -> CltPlank -> List CltPlank
updateCltList cltList ncuts cutDir model parentCltPlank =
    let
        leftSawbladeY =
            if ncuts == 2 then
                model.sawBladeLeft.y

            else if cutDir == 'X' then
                0

            else
                model.sawBladeLeft.y

        topSawbladeX =
            if ncuts == 2 then
                model.sawBladeTop.x

            else if cutDir == 'Y' then
                0

            else
                model.sawBladeTop.x

        plank1 =
            List.singleton <|
                createPlankFromParent leftSawbladeY topSawbladeX 0 0 model.cltMain

        plank2 =
            List.singleton <|
                createPlankFromParent leftSawbladeY (parentCltPlank.length - topSawbladeX) topSawbladeX 0 model.cltMain

        plank3 =
            List.singleton <|
                createPlankFromParent (parentCltPlank.width - leftSawbladeY) (parentCltPlank.length - topSawbladeX) topSawbladeX leftSawbladeY model.cltMain

        plank4 =
            List.singleton <|
                createPlankFromParent (parentCltPlank.width - leftSawbladeY) topSawbladeX 0 leftSawbladeY model.cltMain

        planks =
            List.concat [ plank1, plank2, plank3, plank4 ]
    in
    if ncuts == 2 then
        List.append cltList <| planks

    else if ncuts == 1 then
        List.filter (\c -> c.width /= 0) planks
            |> List.filter (\c -> c.length /= 0)
            |> List.append cltList

    else
        cltList



-- A decoder written to extract the current mouse coordinates and return them in pixels to initiate a state change


decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))



-- Subscriptions are like commands but these are periodically issued functions to the elm runtime which is used to interact
-- with the outside environment (Javascript or other browser servivces)
-- Here we issue multiple subsciptions based on the isOrbiting boolean in model.
-- The mouse event reading as well as animation frame reading are done here.


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isOrbiting && model.animationState /= Off then
        -- If we're currently orbiting and animating, listen for mouse moves and mouse button
        -- up events (to stop orbiting); in a real app we'd probably also want
        -- to listen for page visibility changes to stop orbiting if the user
        -- switches to a different tab or something
        Sub.batch
            [ Browser.Events.onMouseMove decodeMouseMove
            , Browser.Events.onMouseUp (Decode.succeed MouseUp)
            , Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)
            ]

    else if not model.isOrbiting && model.animationState /= Off then
        -- If we're currently orbiting, listen for mouse moves and mouse button
        -- up events (to stop orbiting); in a real app we'd probably also want
        -- to listen for page visibility changes to stop orbiting if the user
        -- switches to a different tab or something
        Sub.batch
            [ Browser.Events.onMouseDown (Decode.succeed MouseDown)
            , Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)
            ]

    else if model.isOrbiting && model.animationState == Off then
        -- If we're currently orbiting, listen for mouse moves and mouse button
        -- up events (to stop orbiting); in a real app we'd probably also want
        -- to listen for page visibility changes to stop orbiting if the user
        -- switches to a different tab or something
        Sub.batch
            [ Browser.Events.onMouseMove decodeMouseMove
            , Browser.Events.onMouseUp (Decode.succeed MouseUp)
            ]

    else
        -- If we're not currently orbiting, just listen for mouse down events
        -- to start orbiting and animation frames
        Sub.batch
            [ Browser.Events.onMouseDown (Decode.succeed MouseDown)
            ]


{-| -}
onScroll : Msg -> Attribute Msg
onScroll msg =
    on "wheel" (decodeMouseScroll msg)


decodeMouseScroll : msg -> Decode.Decoder Msg
decodeMouseScroll msg =
    Decode.map Zoom
        (Decode.succeed MouseWheelEvent
            |> andMap (Decode.field "deltaX" Decode.float)
            |> andMap (Decode.field "deltaY" Decode.float)
            |> andMap (Decode.field "clientX" Decode.float)
            |> andMap (Decode.field "clientY" Decode.float)
        )



-- view
-- This function is used to allocate the new viewport dimensions onto the Window schema part of the model.


didResize : Window -> Int -> Int -> Window
didResize window sw sh =
    { window | sw = round <| toFloat sw * 1.0, sh = round <| toFloat sh * 1.0 }



-- This function is a command issued in the init function to get the viewport sizes by talking to JavaScript. THen they call the state change Message called WindowResize.


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


createCollage : Float -> Float -> List (Shape Msg) -> Html.Html Msg
createCollage w h shapes =
    Svg.svg
        [ SA.width "100%"
        , SA.height "100%"
        , SA.style "position:absolute;top:0px;left:0px;"
        , SA.viewBox
            (String.fromFloat (-w / 2)
                ++ " "
                ++ String.fromFloat (-h / 2)
                ++ " "
                ++ String.fromFloat w
                ++ " "
                ++ String.fromFloat h
            )
        , SA.id "render"
        ]
        (cPath w h
            :: [ Svg.g
                    [ SA.clipPath "url(#cPath)" ]
                    (List.indexedMap
                        (\n -> createSVG (String.fromInt n) w h ident identity ReturnPosition)
                        shapes
                    )
               ]
        )


cPath : Float -> Float -> Svg.Svg Msg
cPath w h =
    Svg.defs []
        [ Svg.clipPath
            [ SA.id "cPath" ]
            [ Svg.rect
                [ SA.width (String.fromFloat w)
                , SA.height (String.fromFloat h)
                , SA.x (String.fromFloat (-w / 2))
                , SA.y (String.fromFloat (-h / 2))
                ]
                []
            ]
        ]


convertCoords :
    Window
    -> ( Float, Float )
    -> ( Float, Float ) -- NOTE:  reversed args
convertCoords gModel ( x, y ) =
    let
        sw =
            gModel.sw

        sh =
            gModel.sh

        cw =
            gModel.cw

        ch =
            gModel.ch

        aspectout =
            if not (sh == 0) then
                toFloat sw / toFloat sh

            else
                4 / 3

        aspectin =
            if not (ch == 0) then
                cw / ch

            else
                4 / 3

        scaledInX =
            aspectout < aspectin

        scaledInY =
            aspectout > aspectin

        cscale =
            if scaledInX then
                toFloat sw / cw

            else if scaledInY then
                toFloat sh / ch

            else
                1
    in
    ( (x - toFloat sw / 2) / cscale
    , (y + toFloat sh / 2) / cscale
    )



-- This is the main view module responsible for displaying the elements on the HTML browser with the help of JavaScript.
-- Here it is slightly modified to output a Browser.Document (which includes Html) type instead of Html type because out elm app type is a Browser.document.


view : Model -> Browser.Document Msg
view model =
    -- Inside here we create different elements required for our 3D graphics environment including lights, cameras, etc.
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

        cameraRotationRate =
            14

        cameraRotationAngle =
            case model.animationState of
                Camera ->
                    Angle.degrees <| cameraRotationRate * model.time

                _ ->
                    Angle.degrees 0

        viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = model.focusAt
                , azimuth = model.azimuth |> Quantity.plus cameraRotationAngle
                , elevation = model.elevation
                , distance = Length.centimeters model.zoom
                }

        viewpointOrtho =
            Viewpoint3d.orbitZ
                { focalPoint = model.focusAt
                , azimuth = model.azimuth |> Quantity.plus cameraRotationAngle
                , elevation = model.elevation
                , distance = Length.centimeters 2000
                }

        viewpointIso =
            Viewpoint3d.orbitZ
                { focalPoint = model.focusAt
                , azimuth = Angle.degrees 45
                , elevation = Viewpoint3d.isometricElevation
                , distance = Length.centimeters 2000
                }

        -- Create a camera with the viewpoint location as mentioned before.
        -- The azimuth and elevation are dynamically taken from the model. These values change when the user drags on the screen.
        camera =
            case model.projection of
                Perspective ->
                    Camera3d.perspective
                        { viewpoint = viewpoint
                        , verticalFieldOfView = Angle.degrees 30
                        }

                Orthographic ->
                    Camera3d.orthographic
                        { viewpoint = viewpointOrtho
                        , viewportHeight = Length.centimeters (model.zoom * 0.5)
                        }

                Isometric ->
                    Camera3d.orthographic
                        { viewpoint = viewpointIso
                        , viewportHeight = Length.centimeters 2000
                        }

        -- Create 3D axes for representing the direction of X, Y and Z
        xAxisMaterial =
            Material.nonmetal
                { baseColor = Color.red
                , roughness = 0.1
                }

        yAxisMaterial =
            Material.nonmetal
                { baseColor = Color.green
                , roughness = 0.1
                }

        zAxisMaterial =
            Material.nonmetal
                { baseColor = Color.blue
                , roughness = 0.1
                }

        xAxisCylinder =
            Scene3d.cylinder xAxisMaterial <|
                Cylinder3d.along Axis3d.x
                    { start = Length.centimeters 0
                    , end = Length.centimeters 2000
                    , radius = Length.centimeters 3
                    }

        yAxisCylinder =
            Scene3d.cylinder yAxisMaterial <|
                Cylinder3d.along Axis3d.y
                    { start = Length.centimeters 0
                    , end = Length.centimeters 2000
                    , radius = Length.centimeters 3
                    }

        zAxisCylinder =
            Scene3d.cylinder zAxisMaterial <|
                Cylinder3d.along Axis3d.z
                    { start = Length.centimeters 0
                    , end = Length.centimeters 2000
                    , radius = Length.centimeters 3
                    }

        -- Grouping the 3 axis into one entity for readability
        axisReference =
            Scene3d.group
                [ xAxisCylinder
                , yAxisCylinder
                , zAxisCylinder
                ]

        -- 2D XY plane Grid
        xyGrid =
            Scene3d.quad (Material.texturedColor model.gridTexture)
                (Point3d.centimeters -2048 2048 -250)
                (Point3d.centimeters 2048 2048 -250)
                (Point3d.centimeters 2048 -2048 -250)
                (Point3d.centimeters -2048 -2048 -250)

        xAxisCltCylinder =
            Scene3d.cylinder xAxisMaterial <|
                Cylinder3d.along (Frame3d.xAxis model.cltMain.cltFrame)
                    { start = Length.centimeters 0
                    , end = Length.centimeters 200
                    , radius = Length.centimeters 0.3
                    }

        yAxisCltCylinder =
            Scene3d.cylinder yAxisMaterial <|
                Cylinder3d.along (Frame3d.yAxis model.cltMain.cltFrame)
                    { start = Length.centimeters 0
                    , end = Length.centimeters 200
                    , radius = Length.centimeters 0.3
                    }

        zAxisCltCylinder =
            Scene3d.cylinder zAxisMaterial <|
                Cylinder3d.along (Frame3d.zAxis model.cltMain.cltFrame)
                    { start = Length.centimeters 0
                    , end = Length.centimeters 200
                    , radius = Length.centimeters 0.3
                    }

        guideLine =
            Wrapper3D.cylinder 4 5000 (Material.metal { baseColor = Color.lightRed, roughness = 0.1 })
                |> Wrapper3D.rotateY3D (degrees 90)
                |> Wrapper3D.move3D ( 2000, 0, 0 )

        rotationRate =
            --Angle.degrees 8 |> Quantity.per Duration.second
            8

        updatedAngle =
            case model.animationState of
                Off ->
                    Angle.degrees 0

                _ ->
                    Angle.degrees <| rotationRate * model.time

        sawBlade =
            Wrapper3D.group3D
                [ Wrapper3D.cylinder 40 4 (Material.metal { baseColor = Color.darkGray, roughness = 0.1 })
                    |> Wrapper3D.scale3D 4
                , spokes 16 (360 / 16)
                    |> Wrapper3D.scale3D 4
                    |> Wrapper3D.rotateY3D (degrees 90)
                    |> Wrapper3D.move3D ( -95, 0, 10 )
                    |> Wrapper3D.rotateZ3D (Angle.inDegrees updatedAngle)
                , guideLine
                ]

        updatedPosition =
            if model.animationState == Cutting && model.isCut then
                let
                    translationRate =
                        400

                    updatedPos =
                        translationRate * model.time
                in
                if (model.cltMain.length - model.sawBladeLeft.x) > updatedPos then
                    updatedPos

                else if (model.cltMain.length - model.sawBladeLeft.x) <= updatedPos then
                    0

                else
                    updatedPos

            else
                0

        -- translationRate |> Quantity.for model.time |> Quantity.negate |> Length.inCentimeters
        camp3dEntities =
            Wrapper3D.renderEntities
                [ sawBlade
                    --top sawblade
                    |> Wrapper3D.scale3D 0.5
                    |> Wrapper3D.rotateY3D (degrees 90)
                    |> Wrapper3D.rotateX3D (degrees 270)
                    -- |> Wrapper3D.move3D ( 100 * Quantity.unwrap xMidpoint, 130, 0 )
                    |> Wrapper3D.move3D
                        ( model.sawBladeTop.x
                        , model.sawBladeTop.y
                            - (if model.cutDir == 'Y' then
                                0

                               else
                                updatedPosition
                              )
                        , model.sawBladeTop.z
                        )
                , sawBlade
                    --left sawblade
                    |> Wrapper3D.scale3D 0.5
                    |> Wrapper3D.rotateX3D (degrees 90)
                    -- |> Wrapper3D.move3D ( -70, 100 * Quantity.unwrap yMidpoint, 0 )
                    |> Wrapper3D.move3D
                        ( model.sawBladeLeft.x
                            + (if model.cutDir == 'X' then
                                0

                               else
                                updatedPosition
                              )
                        , model.sawBladeLeft.y
                        , model.sawBladeLeft.z
                        )
                ]
    in
    -- General structure for writing HTML in document type in elm.
    { title = "CLTCreator"
    , body =
        [ div [ onScroll CheckZoom ]
            [ -- h1 [ style "margin" "0px", style "text-align" "center" ] [ Html.text "CLTCreator" ]
              -- This part includes a Scene3d.custom which is a datatype used to render 3D scenes from the elm-3d-scene library.
              -- we input the values for creating the 3D environment with the values and entities that we have created before.
              Scene3d.custom
                { camera = camera
                , clipDepth = Length.centimeters 0.5
                , dimensions = ( Pixels.int model.window.sw, Pixels.int model.window.sh )
                , antialiasing = Scene3d.multisampling -- Here we use multisampling antialiasing
                , lights = Scene3d.twoLights sunlight overheadLighting
                , exposure = Scene3d.exposureValue 12
                , toneMapping = Scene3d.noToneMapping
                , whiteBalance = Light.daylight
                , background = Scene3d.backgroundColor Color.lightGray
                , entities =
                    [ axisReference
                    , xyGrid
                    , if model.isCut then
                        Scene3d.group <| List.map renderCltPlank model.cltList

                      else
                        renderCltPlank model.cltMain
                    , Scene3d.group camp3dEntities
                    , roundSkybox ( if True then Just model.skyboxTexture else Nothing) 100000 
                    ]
                }   
            , createCollage collageWidth collageHeight <| myShapes model

            --, p [ style "margin" "0px", style "padding" "0px" ] [ Html.text <| Debug.toString model.cltList ]
            ]
        ]
    }



-- MAIN
-- This is a core part of the architecture which is the entry point for the application. It tells the compiler,
-- run time and JavaScript about various points in the architecture and what the functions actually represent.


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
