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
import Array
import Axis3d
import Block3d exposing (axes)
import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events
import Camera3d exposing (Camera3d, viewpoint)
import Color exposing (Color, black, blue, lightOrange)
import Cylinder3d exposing (Cylinder3d)
import Direction3d
import Duration exposing (Duration)
import GraphicSVG exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import Pixels exposing (Pixels, float)
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
import Viewpoint3d exposing (Viewpoint3d)
import WebGL.Texture exposing (Texture)



-- INIT
-- Collage dimensions will be used to generate various overlays later using Scalable vector graphics.


collageWidth =
    240


collageHeight =
    128



-- Boolean to check whether animations are turned on


isAnimating model =
    False



-- Shapes model to render 2D overlay using GraphicSVG


myShapes model =
<<<<<<< HEAD
    [ textBox 20 10 True False [ "Rotate" ] |> move ( 110, 40 ) |> notifyTap (RotateObject 1)
    , textBox 20 10 True False [ "Cut" ] |> move ( 110, 20 )
    , textBox 20 10 True False [ "Play" ] |> move ( 110, 0 )
    , textBox 20 10 True False [ "Focus" ] |> move ( 110, -20 ) |> notifyTap (FocusChange 10 10 0)
    , textBox 20 10 True False [ "Reset" ] |> move ( 110, -40 ) |> notifyTap (FocusChange 0 0 0)
    , textBox 40 20 True True [ "Code Generator: " ] |> move ( -100, -40 )
=======
    [-- textBox 30 35 True False [] |> move (105, 25) |> makeTransparent 0.8 --main, biggest box
    textBox 30 5 True False [ "OPERATIONS" ] |> move ( 105, 40 )
    , textBox 30 5 True False [ "Rotate along X axis" ] |> move ( 105, 35 ) |> notifyTap (RotateObjectX 1) 
    , textBox 30 5 True False [ "Rotate along Y axis" ] |> move ( 105, 30 ) |> notifyTap (RotateObjectY 1) 
    , textBox 30 5 True False [ "Rotate along Z axis" ] |> move ( 105, 25 ) |> notifyTap (RotateObjectZ 1) 
    , textBox 30 5 True False [ "Cut" ] |> move ( 105, 0 ) 
    , textBox 30 5 True False [ "Play" ] |> move ( 105, 20 )
    , textBox 30 5 True False [ "Focus" ] |> move ( 105, 15 ) |> notifyTap (FocusChange 10 10 0) 
    , textBox 30 5 True False [ "Reset" ] |> move ( 105, 10 ) |> notifyTap (FocusChange 0 0 0) 
    , textBox 40 20 True True [ "Your Code: " ] |> move ( -100, -40 ) 
>>>>>>> c5dc4e6 (folder structure fix)
    ]



-- Textbox overlay template


textBox width height isHighlighted isSelectable chars =
<<<<<<< HEAD
    [ rect width height |> filled white
    , if isSelectable then
        GraphicSVG.text (String.join "" <| List.reverse chars)
            |> centered
            |> GraphicSVG.size 4
            -- |> selectable
            |> filled GraphicSVG.black
            |> clip (rect width height |> ghost)

      else
        GraphicSVG.text (String.join "" <| List.reverse chars)
            |> centered
            |> GraphicSVG.size 4
            |> filled GraphicSVG.black
            |> clip (rect width height |> ghost)
    , rect width height
        |> outlined (solid 1)
            (if isHighlighted then
                rgb 0 0 255
=======
    [ rect width height |> filled white |> makeTransparent 0.8
    , if isSelectable then
        GraphicSVG.text (String.join "" <| List.reverse chars)
            |> centered
            |> GraphicSVG.size 3
            -- |> selectable
            |> filled GraphicSVG.black
            |> clip (rect width height |> ghost)
         
      else
        GraphicSVG.text (String.join "" <| List.reverse chars)
            |> centered
            |> GraphicSVG.size 3
            |> filled GraphicSVG.black
            |> clip (rect width height |> ghost)
    , rect width height
        |> outlined (solid 0.2)
            (if isHighlighted then
                rgb 0 0 0
>>>>>>> c5dc4e6 (folder structure fix)

             else
                charcoal
            )
    ]
        |> group



-- Type to handle world coordinates in the 3d scene.


type WorldCoordinates
    = WorldCoordinates



-- Window is a type alias for a custom record used to hold values for Collage and Scene Dimensions.


type alias Window =
    { cw : Float
    , ch : Float
    , sw : Int
    , sh : Int
    }



-- Main Model (State variable) type which is used to store current state values for the application.


type alias Model =
    { window : Window
    , elapsedTime : Duration
    , azimuth : Angle
    , elevation : Angle
    , focusAt : Point3d.Point3d Meters WorldCoordinates
    , isOrbiting : Bool
    , cltRotationAngle : Angle
    , cltMesh1 : Mesh.Unlit WorldCoordinates
    , cltMesh2 : Mesh.Unlit WorldCoordinates
    , cltTopTexture : Material.Texture Color.Color
    , cltSideTexture : Material.Texture Color.Color
    , gridTexture : Material.Texture Color.Color
    }



-- Initialization of the model state variable as well as the app on initial load.
-- The initialization contains the mesh information for the CLT slab which holds the vector adn uv coordinates in Point3d centimeters and float respectively.


init : () -> ( Model, Cmd Msg )
init () =
    let
        -- Combined mesh with upper + lower rectangular meshes and a Strip mesh for the sides.
        -- The mesh is currently set to default clt plank dimensions to ensure uniformity.
        -- Variable Plank Mesh function will be added as a feature after the base functionality.
        -- The combined mesh is made up of two triangular meshes of indexed and strip respectively.
        mesh =
            Mesh.texturedTriangles <|
                TriangularMesh.combine [ upperMesh, lowerMesh ]

        upperMesh =
            TriangularMesh.indexed
                (Array.fromList
                    [ { position = Point3d.centimeters 0 0 0.4, uv = ( 0.0, 0.0 ) } -- 0
                    , { position = Point3d.centimeters 5 0 0.4, uv = ( 1.0, 0.0 ) } -- 1
                    , { position = Point3d.centimeters 5 3 0.4, uv = ( 1.0, 1.0 ) } -- 2
                    , { position = Point3d.centimeters 0 3 0.4, uv = ( 0.0, 1.0 ) } -- 3
                    , { position = Point3d.centimeters 0 0 0.4, uv = ( 0.0, 0.0 ) }
                    ]
                )
                [ ( 0, 1, 2 )
                , ( 2, 3, 0 )
                ]

        lowerMesh =
            TriangularMesh.indexed
                (Array.fromList
                    [ { position = Point3d.centimeters 0 0 0, uv = ( 0.0, 0.0 ) } -- 0
                    , { position = Point3d.centimeters 5 0 0, uv = ( 1.0, 0.0 ) } -- 1
                    , { position = Point3d.centimeters 5 3 0, uv = ( 1.0, 1.0 ) } -- 2
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
                    [ { position = Point3d.centimeters 0 0 0.4, uv = ( 0.0, 0.0 ) } -- 0
                    , { position = Point3d.centimeters 5 0 0.4, uv = ( 1.0, 0.0 ) } -- 1
                    , { position = Point3d.centimeters 5 3 0.4, uv = ( 0.0, 0.0 ) } -- 2
                    , { position = Point3d.centimeters 0 3 0.4, uv = ( 1.0, 0.0 ) } -- 3
                    , { position = Point3d.centimeters 0 0 0.4, uv = ( 0.0, 0.0 ) }
                    ]
                    [ { position = Point3d.centimeters 0 0 0, uv = ( 0.0, 1.0 ) } -- 0
                    , { position = Point3d.centimeters 5 0 0, uv = ( 1.0, 1.0 ) } -- 1
                    , { position = Point3d.centimeters 5 3 0, uv = ( 0.0, 1.0 ) } -- 2
                    , { position = Point3d.centimeters 0 3 0, uv = ( 1.0, 1.0 ) } -- 3
                    , { position = Point3d.centimeters 0 0 0, uv = ( 0.0, 1.0 ) }
                    ]
    in
    -- In the init functio nwe store the previously created mesh and other values since creation of a mesh is an expensive operation
    --   and changing the mesh frequently causes optimization issues.
    -- This is why we store the mesh in the state change variable instead of calculating and remaking the mesh at every instance.
    -- We also store the texture values for the created meshes to apply them later in the view function
    -- elapsedTime is a data type used to handle time w.r.t Animation and frames.
    -- azimuth and elevation hold the values required to position the camera. These are dynamically changed while the user drags on the screen.
    -- isOrbiting is a boolean which holds true if the user wants to drag the screen else it is false.
    -- cltMeshx and clt_Texture hold mesh and texture values as discussed above.
    -- Coming to the commands, we are issuing 3 separate commands on initialisation.
    -- getViewportSize is used to get the current viewport dimensions of the browser from javascript.
    -- The other two commands are used to make a request to the github repositories which hold the textures.
    ( { window = { cw = collageWidth, ch = collageHeight, sw = 0, sh = 0 }
      , elapsedTime = Quantity.zero
      , azimuth = Angle.degrees 45
      , elevation = Angle.degrees 30
      , focusAt = Point3d.centimeters 0 0 0
      , isOrbiting = False
      , cltRotationAngle = Angle.degrees 0
      , cltMesh1 = mesh
      , cltMesh2 = rawStripMesh
      , cltTopTexture = Material.constant Color.black
      , cltSideTexture = Material.constant Color.black
      , gridTexture = Material.constant Color.black
      }
    , Cmd.batch
        [ getViewportSize

        -- Important to note that we render the below textures using trilinearFiltering texture/ image filtering.
        , Task.attempt (GotTexture "top") (Material.loadWith Material.bilinearFiltering cltTopTextureURL)
        , Task.attempt (GotTexture "side") (Material.loadWith Material.bilinearFiltering cltSideTextureURL)
        , Task.attempt (GotTexture "grid") (Material.loadWith Material.trilinearFiltering gridTextureURL)
        ]
    )



-- URLs for the CLT textures and Grid


cltTopTextureURL : String
cltTopTextureURL =
    "https://raw.githubusercontent.com/An-u-rag/elm-3d-clt-playground/main/clt-textures/clt1.png"


cltSideTextureURL : String
cltSideTextureURL =
    "https://raw.githubusercontent.com/An-u-rag/elm-3d-clt-playground/main/clt-textures/crosssection-2.png"


gridTextureURL : String
gridTextureURL =
<<<<<<< HEAD
    "https://raw.githubusercontent.com/An-u-rag/elm-3d-clt-playground/main/GraphPaperTextures/gridx64-1024.png"
=======
    "https://raw.githubusercontent.com/An-u-rag/elm-3d-clt-playground/main/GraphPaperTextures/GraphPaper1024-grey.png"
>>>>>>> c5dc4e6 (folder structure fix)



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
    | WindowResize (Maybe ( Int, Int ))
    | ReturnPosition (( Float, Float ) -> Msg) ( Float, Float )
    | MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | GotTexture String (Result WebGL.Texture.Error (Material.Texture Color.Color))
    | FocusChange Float Float Float
<<<<<<< HEAD
    | RotateObject Int
=======
    | RotateObjectX Int
    | RotateObjectY Int
    | RotateObjectZ Int
>>>>>>> c5dc4e6 (folder structure fix)
    | NoOp



-- The actual update function which has case by case for each of the state change initiators from "Msg" datatype above.
-- THis function dicatates WHAT to change, the actual action in the state change, and HOW to change it.


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Tick is used to state change for every animation frame.
        Tick duration ->
            ( { model | elapsedTime = Quantity.plus duration model.elapsedTime }, Cmd.none )

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
                ( { model | azimuth = newAzimuth, elevation = newElevation }, Cmd.none )

            else
                ( model, Cmd.none )

        -- Setting the retrieved texture values into the model.
        GotTexture textureType (Ok texture) ->
            if textureType == "top" then
                -- Successfully loaded the texture
                ( { model | cltTopTexture = texture }, Cmd.none )

            else if textureType == "grid" then
                ( { model | gridTexture = texture }, Cmd.none )

            else
                ( { model | cltSideTexture = texture }, Cmd.none )

        GotTexture textureType (Err error) ->
            if textureType == "top" then
                ( { model | cltTopTexture = Material.constant Color.blue }, Cmd.none )

            else if textureType == "grid" then
                ( { model | gridTexture = Material.constant Color.blue }, Cmd.none )

            else
                ( { model | cltSideTexture = Material.constant Color.blue }, Cmd.none )

        FocusChange x y z ->
            ( { model | focusAt = Point3d.centimeters x y z }, Cmd.none )

<<<<<<< HEAD
        RotateObject id ->
=======
        RotateObjectX id ->
            ( { model | cltRotationAngle = Quantity.plus model.cltRotationAngle (Angle.degrees 90) }, Cmd.none )

        RotateObjectY id ->
            ( { model | cltRotationAngle = Quantity.plus model.cltRotationAngle (Angle.degrees 90) }, Cmd.none )
        
        RotateObjectZ id ->
>>>>>>> c5dc4e6 (folder structure fix)
            ( { model | cltRotationAngle = Quantity.plus model.cltRotationAngle (Angle.degrees 90) }, Cmd.none )

        -- Default catch to make no change to model/state.
        NoOp ->
            ( model, Cmd.none )



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
        -- to start orbiting and animation frames
        Sub.batch
            [ Browser.Events.onMouseDown (Decode.succeed MouseDown)
            , Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)
            ]



-- view
-- This function is used to allocate the new viewport dimensions onto the Window schema part of the model.


didResize : Window -> Int -> Int -> Window
didResize window sw sh =
    { window | sw = round <| toFloat sw * 0.99, sh = round <| toFloat sh * 0.95 }



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

        viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = model.focusAt
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.centimeters 20
                }

        -- Create a camera with the viewpoint location as mentioned before.
        -- The azimuth and elevation are dynamically taken from the model. These values change when the user drags on the screen.
        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
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
                    , end = Length.centimeters 32
                    , radius = Length.centimeters 0.05
                    }

        yAxisCylinder =
            Scene3d.cylinder yAxisMaterial <|
                Cylinder3d.along Axis3d.y
                    { start = Length.centimeters 0
                    , end = Length.centimeters 32
                    , radius = Length.centimeters 0.05
                    }

        zAxisCylinder =
            Scene3d.cylinder zAxisMaterial <|
                Cylinder3d.along Axis3d.z
                    { start = Length.centimeters 0
                    , end = Length.centimeters 32
                    , radius = Length.centimeters 0.05
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
                (Point3d.centimeters -32 32 -0.25)
                (Point3d.centimeters 32 32 -0.25)
                (Point3d.centimeters 32 -32 -0.25)
                (Point3d.centimeters -32 -32 -0.25)

        -- CLT plank
        cltPlank =
            Scene3d.group
                [ Scene3d.mesh (Material.texturedColor model.cltTopTexture) model.cltMesh1
                , Scene3d.mesh (Material.texturedColor model.cltSideTexture) model.cltMesh2
                ]

<<<<<<< HEAD
        rotationAxis =
            Axis3d.through (Point3d.meters 0 0 0) Direction3d.x
=======
        rotationAxisX =
            Axis3d.through (Point3d.meters 0 0 0) Direction3d.x

        rotationAxisY =
            Axis3d.through (Point3d.meters 0 0 0) Direction3d.y

        rotationAxisZ =
            Axis3d.through (Point3d.meters 0 0 0) Direction3d.z
>>>>>>> c5dc4e6 (folder structure fix)
    in
    -- General structure for writing HTML in document type in elm.
    { title = "CLTCreator"
    , body =
        [ div []
            [ h1 [ style "margin" "0px", style "text-align" "center" ] [ Html.text "CLTCreator" ]

            -- This part includes a Scene3d.custom which is a datatype used to render 3D scenes from the elm-3d-scene library.
            -- we input the values for creating the 3D environment with the values and entities that we have created before.
            , Scene3d.custom
                { camera = camera
                , clipDepth = Length.centimeters 0.5
                , dimensions = ( Pixels.int model.window.sw, Pixels.int model.window.sh )
                , antialiasing = Scene3d.multisampling -- Here we use multisampling antialiasing
                , lights = Scene3d.twoLights sunlight overheadLighting
                , exposure = Scene3d.exposureValue 12
                , toneMapping = Scene3d.noToneMapping
                , whiteBalance = Light.daylight
                , background = Scene3d.backgroundColor Color.grey
                , entities =
                    [ axisReference
                    , xyGrid
<<<<<<< HEAD
                    , cltPlank |> Scene3d.rotateAround rotationAxis model.cltRotationAngle
=======
                    , cltPlank |> Scene3d.rotateAround rotationAxisX model.cltRotationAngle 
>>>>>>> c5dc4e6 (folder structure fix)
                    ]
                }
            , createCollage collageWidth collageHeight <| myShapes model
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
