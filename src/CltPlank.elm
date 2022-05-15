module CltPlank exposing (..)

import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d
import Block3d exposing (axes)
import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events
import Camera3d exposing (Camera3d, viewpoint)
import Color exposing (Color, black, blue, lightOrange)
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



-- Type to handle world coordinates in the 3d scene.


type WorldCoordinates
    = WorldCoordinates


type LocalCoordinates
    = LocalCoordinates


type alias Frame =
    Frame3d Meters WorldCoordinates { defines : LocalCoordinates }


type alias VertexData =
    { position : Point3d.Point3d Meters WorldCoordinates
    , uv : ( Float, Float )
    }



-- Clt Model to hold all values related to that clt entity.


type alias CltPlank =
    { width : Float
    , length : Float
    , height : Float
    , offsetX : Float
    , offsetY : Float
    , rotationAngleX : Angle
    , rotationAngleY : Angle
    , rotationAngleZ : Angle
    , centerPoint : Point3d Meters WorldCoordinates
    , cltFrame : Frame
    , indexedMesh : Mesh.Unlit WorldCoordinates
    , stripMesh : Mesh.Unlit WorldCoordinates
    , cltTopTexture : Material.Texture Color.Color
    , cltSideTexture : Material.Texture Color.Color
    }


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



-- Combined mesh with upper + lower rectangular meshes and a Strip mesh for the sides (with variable widths and lengths)
-- The combined mesh is made up of two triangular meshes of indexed and strip respectively.


meshV width length height =
    Mesh.texturedTriangles <|
        TriangularMesh.combine [ upperMeshV width length height, lowerMeshV width length ]


upperMeshV width length height =
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


lowerMeshV width length =
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


rawStripMeshV width length height =
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


stripMeshV width length height =
    Mesh.texturedTriangles <| rawStripMeshV width length height


addVertices : Maybe VertexData -> Maybe VertexData -> Point3d Meters WorldCoordinates
addVertices a b =
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


calcCenter : TriangularMesh VertexData -> TriangularMesh VertexData -> Point3d Meters WorldCoordinates
calcCenter uMesh sMesh =
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
            Point3d.xCoordinate (addVertices a b)

        cy =
            Point3d.yCoordinate (addVertices a d)

        cz =
            Point3d.zCoordinate (addVertices a e)
    in
    Point3d.xyz cx cy cz



-- deprecated


mesh =
    Mesh.texturedTriangles <|
        TriangularMesh.combine [ upperMesh, lowerMesh ]


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


stripMesh =
    Mesh.texturedTriangles <| rawStripMesh
