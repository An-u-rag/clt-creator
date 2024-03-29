module CltPlank exposing (CltPlank, cltSideTextureURL, cltTopTextureURL, createCltPlank, createCltPlankRaw, cutAndCreateCltPlank, cutPlank, defaultPlank, getCenterPoint, renderCltPlank, resetPlank, rotateClt, updateCltTexture)

import Angle exposing (Angle)
import Array exposing (Array)
import Block3d exposing (axes)
import Browser.Dom exposing (Viewport, getViewport)
import Camera3d exposing (Camera3d, viewpoint)
import Color exposing (Color, black, blue, lightOrange)
import Cylinder3d exposing (Cylinder3d)
import Duration exposing (Duration)
import Frame3d exposing (Frame3d)
import GraphicSVG exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
import TriangularMesh exposing (TriangularMesh, vertex)
import Vector3d exposing (..)
import Viewpoint3d exposing (Viewpoint3d)
import WebGL.Texture exposing (Texture)
import Wrapper3D exposing (Object, fromEntity)


type alias Frame worldCoordinates localCoordinates =
    Frame3d Meters worldCoordinates { defines : localCoordinates }


type alias VertexData coordinates =
    { position : Point3d.Point3d Meters coordinates
    , uv : ( Float, Float )
    }



-- Clt Model to hold all values related to that clt entity.


type alias CltPlank worldCoordinates localCoordinates =
    { width : Float
    , length : Float
    , height : Float
    , offsetX : Float
    , offsetY : Float
    , rotationAngleX : Angle
    , rotationAngleY : Angle
    , rotationAngleZ : Angle
    , centerPoint : Point3d Meters worldCoordinates
    , cltFrame : Frame worldCoordinates localCoordinates
    , indexedMesh : Mesh.Unlit worldCoordinates
    , stripMesh : Mesh.Unlit worldCoordinates
    , cltTopTexture : Material.Texture Color.Color
    , cltSideTexture : Material.Texture Color.Color
    }


cltTopTextureURL : String
cltTopTextureURL =
    "https://raw.githubusercontent.com/An-u-rag/elm-3d-clt-playground/main/clt-textures/clt1.png"


cltSideTextureURL : String
cltSideTextureURL =
    "https://raw.githubusercontent.com/An-u-rag/elm-3d-clt-playground/main/clt-textures/crosssection-2.png"


updateCltTexture : CltPlank worldCoordinates localCoordinates -> String -> Material.Texture Color.Color -> CltPlank worldCoordinates localCoordinates
updateCltTexture clt attrib value =
    case attrib of
        "TopTexture" ->
            { clt | cltTopTexture = value }

        "SideTexture" ->
            { clt | cltSideTexture = value }

        _ ->
            clt


defaultPlank : CltPlank worldCoordinates localCoordinates
defaultPlank =
    { width = 0
    , length = 0
    , height = 0
    , offsetX = 0
    , offsetY = 0
    , rotationAngleX = Angle.degrees 0
    , rotationAngleY = Angle.degrees 0
    , rotationAngleZ = Angle.degrees 0
    , centerPoint = Point3d.centimeters 0 0 0
    , cltFrame = Frame3d.atPoint (Point3d.centimeters 0 0 0)
    , indexedMesh = mesh
    , stripMesh = stripMesh
    , cltTopTexture = Material.constant Color.black
    , cltSideTexture = Material.constant Color.black
    }


createCltPlankRaw : Float -> Float -> Float -> CltPlank worldCoordinates localCoordinates
createCltPlankRaw width length height =
    let
        mainIndexedMesh =
            cltPlankMeshV width length height

        mainStripMesh =
            cltPlankStripMeshV width length height

        centerPoint =
            calculateCltPlankCenter (cltPlankXYUpperMeshV width length height) (cltPlankRawStripMeshV width length height)
    in
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
    , indexedMesh = mainIndexedMesh
    , stripMesh = mainStripMesh
    , cltTopTexture = Material.constant Color.black
    , cltSideTexture = Material.constant Color.black
    }


createCltPlank : Float -> Float -> Float -> Object worldCoordinates
createCltPlank width length height =
    let
        mainIndexedMesh =
            cltPlankMeshV width length height

        mainStripMesh =
            cltPlankStripMeshV width length height

        centerPoint =
            calculateCltPlankCenter (cltPlankXYUpperMeshV width length height) (cltPlankRawStripMeshV width length height)
    in
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
    , indexedMesh = mainIndexedMesh
    , stripMesh = mainStripMesh
    , cltTopTexture = Material.constant Color.black
    , cltSideTexture = Material.constant Color.black
    }
        |> renderCltPlank
        |> fromEntity ( width, length, height )


getCenterPoint : CltPlank worldCoordinates localCoordinates -> Point3d Meters worldCoordinates
getCenterPoint clt =
    clt.centerPoint



-- This function will take the ParentPlank, X and Y values, number of cuts, cut direction and offsets as arguments.
-- This will call the cutAndCreateCltPlank function.
-- Returns a List of CltPlanks as a result which contaisn the cut planks.


cutPlank : CltPlank worldCoordinates localCoordinates -> Float -> Float -> Int -> String -> List (CltPlank worldCoordinates localCoordinates)
cutPlank parentPlank xPos yPos ncuts cutDir =
    let
        cltList =
            []

        leftSawbladeY =
            if ncuts == 2 then
                yPos

            else if cutDir == "X" then
                0

            else
                yPos

        topSawbladeX =
            if ncuts == 2 then
                xPos

            else if cutDir == "Y" then
                0

            else
                xPos

        plank1 =
            List.singleton <|
                cutAndCreateCltPlank leftSawbladeY topSawbladeX 0 0 parentPlank

        plank2 =
            List.singleton <|
                cutAndCreateCltPlank leftSawbladeY (parentPlank.length - topSawbladeX) topSawbladeX 0 parentPlank

        plank3 =
            List.singleton <|
                cutAndCreateCltPlank (parentPlank.width - leftSawbladeY) (parentPlank.length - topSawbladeX) topSawbladeX leftSawbladeY parentPlank

        plank4 =
            List.singleton <|
                cutAndCreateCltPlank (parentPlank.width - leftSawbladeY) topSawbladeX 0 leftSawbladeY parentPlank
    in
    if ncuts == 2 then
        List.append cltList <| List.concat [ plank1, plank2, plank3, plank4 ]

    else if ncuts == 1 then
        List.filter (\c -> c.width /= 0) (List.concat [ plank1, plank2, plank3, plank4 ])
            |> List.filter (\c -> c.length /= 0)
            |> List.append cltList

    else
        cltList


cutAndCreateCltPlank : Float -> Float -> Float -> Float -> CltPlank worldCoordinates localCoordinates -> CltPlank worldCoordinates localCoordinates
cutAndCreateCltPlank width length offsetX offsetY parentCltPlank =
    let
        height =
            parentCltPlank.height

        center =
            Point3d.toRecord Length.inCentimeters <| calculateCltPlankCenter (cltPlankXYUpperMeshV width length height) (cltPlankRawStripMeshV width length height)

        centerPoint =
            Point3d.xyz
                (Length.centimeters (center.x + offsetX))
                (Length.centimeters (center.y + offsetY))
                (Length.centimeters center.z)
    in
    { width = width
    , length = length
    , height = height
    , offsetX = offsetX
    , offsetY = offsetY
    , rotationAngleX = parentCltPlank.rotationAngleX
    , rotationAngleY = parentCltPlank.rotationAngleY
    , rotationAngleZ = parentCltPlank.rotationAngleZ
    , centerPoint = centerPoint
    , cltFrame = Frame3d.atPoint centerPoint
    , indexedMesh = cltPlankMeshV width length height
    , stripMesh = cltPlankStripMeshV width length height
    , cltTopTexture = parentCltPlank.cltTopTexture
    , cltSideTexture = parentCltPlank.cltSideTexture
    }


resetPlank : CltPlank worldCoordinates localCoordinates -> CltPlank worldCoordinates localCoordinates
resetPlank clt =
    { width = clt.width
    , length = clt.length
    , height = clt.height
    , offsetX = 0
    , offsetY = 0
    , rotationAngleX = Angle.degrees 0
    , rotationAngleY = Angle.degrees 0
    , rotationAngleZ = Angle.degrees 0
    , centerPoint = clt.centerPoint
    , cltFrame = Frame3d.atPoint clt.centerPoint
    , indexedMesh = clt.indexedMesh
    , stripMesh = clt.stripMesh
    , cltTopTexture = clt.cltTopTexture
    , cltSideTexture = clt.cltSideTexture
    }


rotateClt : CltPlank worldCoordinates localCoordinates -> Char -> CltPlank worldCoordinates localCoordinates
rotateClt clt axis =
    if axis == 'X' || axis == 'x' then
        { clt
            | rotationAngleX = Quantity.plus clt.rotationAngleX (Angle.degrees 90)
        }

    else if axis == 'Y' || axis == 'y' then
        { clt
            | rotationAngleY = Quantity.plus clt.rotationAngleY (Angle.degrees 90)
        }

    else if axis == 'Z' || axis == 'z' then
        { clt
            | rotationAngleZ = Quantity.plus clt.rotationAngleZ (Angle.degrees 90)
        }

    else
        clt



-- Combined mesh with upper + lower rectangular meshes and a Strip mesh for the sides (with variable widths and lengths)
-- The combined mesh is made up of two triangular meshes of indexed and strip respectively.


cltPlankMeshV : Float -> Float -> Float -> Mesh.Unlit coordinates
cltPlankMeshV width length height =
    Mesh.texturedTriangles <|
        TriangularMesh.combine [ cltPlankXYUpperMeshV width length height, cltPlankXYLowerMeshV width length ]


cltPlankXYUpperMeshV : Float -> Float -> Float -> TriangularMesh (VertexData coordinates)
cltPlankXYUpperMeshV width length height =
    TriangularMesh.indexed
        (Array.fromList
            [ { position = Point3d.centimeters 0 0 height, uv = ( 0.0, 0.0 ) } -- 0
            , { position = Point3d.centimeters length 0 height, uv = ( 1.0, 0.0 ) } -- 1
            , { position = Point3d.centimeters length width height, uv = ( 1.0, 1.0 ) } -- 2
            , { position = Point3d.centimeters 0 width height, uv = ( 0.0, 1.0 ) } -- 3
            , { position = Point3d.centimeters 0 0 height, uv = ( 0.0, 0.0 ) }
            ]
        )
        [ ( 0, 1, 2 )
        , ( 2, 3, 0 )
        ]


cltPlankXYLowerMeshV : Float -> Float -> TriangularMesh (VertexData coordinates)
cltPlankXYLowerMeshV width length =
    TriangularMesh.indexed
        (Array.fromList
            [ { position = Point3d.centimeters 0 0 0, uv = ( 0.0, 0.0 ) } -- 0
            , { position = Point3d.centimeters length 0 0, uv = ( 1.0, 0.0 ) } -- 1
            , { position = Point3d.centimeters length width 0, uv = ( 1.0, 1.0 ) } -- 2
            , { position = Point3d.centimeters 0 width 0, uv = ( 0.0, 1.0 ) } -- 3
            , { position = Point3d.centimeters 0 0 0, uv = ( 0.0, 0.0 ) }
            ]
        )
        [ ( 0, 1, 2 )
        , ( 2, 3, 0 )
        ]


cltPlankRawStripMeshV : Float -> Float -> Float -> TriangularMesh (VertexData coordinates)
cltPlankRawStripMeshV width length height =
    TriangularMesh.strip
        [ { position = Point3d.centimeters 0 0 height, uv = ( 0.0, 0.0 ) } -- 0
        , { position = Point3d.centimeters length 0 height, uv = ( 1.0, 0.0 ) } -- 1
        , { position = Point3d.centimeters length width height, uv = ( 0.0, 0.0 ) } -- 2
        , { position = Point3d.centimeters 0 width height, uv = ( 1.0, 0.0 ) } -- 3
        , { position = Point3d.centimeters 0 0 height, uv = ( 0.0, 0.0 ) }
        ]
        [ { position = Point3d.centimeters 0 0 0, uv = ( 0.0, 1.0 ) } -- 0
        , { position = Point3d.centimeters length 0 0, uv = ( 1.0, 1.0 ) } -- 1
        , { position = Point3d.centimeters length width 0, uv = ( 0.0, 1.0 ) } -- 2
        , { position = Point3d.centimeters 0 width 0, uv = ( 1.0, 1.0 ) } -- 3
        , { position = Point3d.centimeters 0 0 0, uv = ( 0.0, 1.0 ) }
        ]


cltPlankStripMeshV : Float -> Float -> Float -> Mesh.Unlit coordinates
cltPlankStripMeshV width length height =
    Mesh.texturedTriangles <| cltPlankRawStripMeshV width length height


getMidpointOfVertices : Maybe (VertexData worldCoordinates) -> Maybe (VertexData worldCoordinates) -> Point3d Meters worldCoordinates
getMidpointOfVertices a b =
    let
        ea =
            Maybe.withDefault
                { position = Point3d.centimeters 0 0 0, uv = ( 0.0, 0.0 ) }
                a

        eb =
            Maybe.withDefault
                { position = Point3d.centimeters 0 0 0, uv = ( 0.0, 0.0 ) }
                b
    in
    Point3d.midpoint ea.position eb.position


calculateCltPlankCenter : TriangularMesh (VertexData worldCoordinates) -> TriangularMesh (VertexData worldCoordinates) -> Point3d Meters worldCoordinates
calculateCltPlankCenter uMesh sMesh =
    let
        a =
            TriangularMesh.vertex 0 uMesh

        b =
            TriangularMesh.vertex 1 uMesh

        d =
            TriangularMesh.vertex 3 uMesh

        e =
            TriangularMesh.vertex 3 sMesh

        cx =
            Point3d.xCoordinate (getMidpointOfVertices a b)

        cy =
            Point3d.yCoordinate (getMidpointOfVertices a d)

        cz =
            Point3d.zCoordinate (getMidpointOfVertices a e)
    in
    Point3d.xyz cx cy cz



-- Render a plank in view


renderCltPlank : CltPlank worldCoordinates localCoordinates -> Scene3d.Entity worldCoordinates
renderCltPlank clt =
    let
        rotationAxisX =
            Frame3d.xAxis clt.cltFrame

        rotationAxisY =
            Frame3d.yAxis clt.cltFrame

        rotationAxisZ =
            Frame3d.zAxis clt.cltFrame

        ox =
            clt.offsetX / 100

        oy =
            clt.offsetY / 100
    in
    Scene3d.group
        [ Scene3d.mesh (Material.texturedColor clt.cltTopTexture) clt.indexedMesh
        , Scene3d.mesh (Material.texturedColor clt.cltSideTexture) clt.stripMesh
        ]
        |> Scene3d.translateBy (Vector3d.meters ox oy 0)
        |> Scene3d.rotateAround rotationAxisX clt.rotationAngleX
        |> Scene3d.rotateAround rotationAxisY clt.rotationAngleY
        |> Scene3d.rotateAround rotationAxisZ clt.rotationAngleZ



-- Being used for default plank


mesh : Mesh.Unlit coordinates
mesh =
    Mesh.texturedTriangles <|
        TriangularMesh.combine [ upperMesh, lowerMesh ]


upperMesh : TriangularMesh (VertexData coordinates)
upperMesh =
    TriangularMesh.indexed
        (Array.fromList
            [ { position = Point3d.centimeters 0 0 4, uv = ( 0.0, 0.0 ) } -- 0
            , { position = Point3d.centimeters 152 0 4, uv = ( 1.0, 0.0 ) } -- 1
            , { position = Point3d.centimeters 152 30.5 4, uv = ( 1.0, 1.0 ) } -- 2
            , { position = Point3d.centimeters 0 30.5 4, uv = ( 0.0, 1.0 ) } -- 3
            , { position = Point3d.centimeters 0 0 4, uv = ( 0.0, 0.0 ) }
            ]
        )
        [ ( 0, 1, 2 )
        , ( 2, 3, 0 )
        ]


lowerMesh : TriangularMesh (VertexData coordinates)
lowerMesh =
    TriangularMesh.indexed
        (Array.fromList
            [ { position = Point3d.centimeters 0 0 0, uv = ( 0.0, 0.0 ) } -- 0
            , { position = Point3d.centimeters 152 0 0, uv = ( 1.0, 0.0 ) } -- 1
            , { position = Point3d.centimeters 152 30 0, uv = ( 1.0, 1.0 ) } -- 2
            , { position = Point3d.centimeters 0 30 0, uv = ( 0.0, 1.0 ) } -- 3
            , { position = Point3d.centimeters 0 0 0, uv = ( 0.0, 0.0 ) }
            ]
        )
        [ ( 0, 1, 2 )
        , ( 2, 3, 0 )
        ]


rawStripMesh : TriangularMesh (VertexData coordinates)
rawStripMesh =
    TriangularMesh.strip
        [ { position = Point3d.centimeters 0 0 4, uv = ( 0.0, 0.0 ) } -- 0
        , { position = Point3d.centimeters 152 0 4, uv = ( 1.0, 0.0 ) } -- 1
        , { position = Point3d.centimeters 152 30 4, uv = ( 0.0, 0.0 ) } -- 2
        , { position = Point3d.centimeters 0 30 4, uv = ( 1.0, 0.0 ) } -- 3
        , { position = Point3d.centimeters 0 0 4, uv = ( 0.0, 0.0 ) }
        ]
        [ { position = Point3d.centimeters 0 0 0, uv = ( 0.0, 1.0 ) } -- 0
        , { position = Point3d.centimeters 152 0 0, uv = ( 1.0, 1.0 ) } -- 1
        , { position = Point3d.centimeters 152 30 0, uv = ( 0.0, 1.0 ) } -- 2
        , { position = Point3d.centimeters 0 30 0, uv = ( 1.0, 1.0 ) } -- 3
        , { position = Point3d.centimeters 0 0 0, uv = ( 0.0, 1.0 ) }
        ]


stripMesh : Mesh.Unlit coordinates
stripMesh =
    Mesh.texturedTriangles <| rawStripMesh
