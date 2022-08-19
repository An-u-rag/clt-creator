module Wrapper3D exposing
    ( Collider(..)
    , CustomMesh(..)
    , Dimension
    , GeneratedMesh
    , MeshStore
    , Mold
    , Object(..)
    , ShapeApproximation(..)
    , animationPieces
    , arrowStartingAt
    , box
    , boxOutline
    , boxU
    , cone
    , coneU
    , constantColourTexture
    , constantTexture
    , cube
    , cubeU
    , customMat
    , customObject
    , customPolygon
    , cylinder
    , cylinderStartingAt
    , cylinderU
    , ellipsoid
    , fromEntity
    , generateDynamicMesh
    , generateEllipsoid
    , generatePolyCone
    , generatePolyCylinder
    , generateRing
    , generateTexturedCylinder
    , generateTruncatedCone
    , getCentre
    , getCollider
    , getMeshName
    , getPosition
    , getVolume
    , group3D
    , matte
    , metallic
    , move3D
    , move3DU
    , placeObject
    , plastic
    , pointLight
    , polyCone
    , polyCylinder
    , polygon3D
    , quad3D
    , rectangle3D
    , relativeP
    , renderCollider
    , renderEntities
    , repeatDistance
    , repeatDuration
    , ring
    , rotate3D
    , rotate3DU
    , rotateX3D
    , rotateY3D
    , rotateZ3D
    , scale3D
    , scaleTo3D
    , sphere
    , sphereU
    , square3D
    , textured
    , texturedCylinder
    , truncatedCone
    , unwrapQ
    , withOverlay
    )

import Angle exposing (Angle)
import Arc2d
import Arc3d
import Array exposing (Array)
import Axis3d
import Bitwise
import Block3d exposing (Block3d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle3d
import Color exposing (Color)
import Cone3d exposing (Cone3d)
import Cylinder3d exposing (Cylinder3d)
import Dict exposing (Dict)
import Direction2d
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Frame3d
import GraphicSVG as G exposing (Shape)
import GraphicSVG.Widget as Widget
import Html exposing (Html)
import Html.Attributes as HA exposing (style)
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Length, Meters)
import LineSegment3d
import LuminousFlux exposing (LuminousFlux)
import Parameter1d
import Pixels exposing (Pixels)
import Point2d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Light as Light exposing (Chromaticity, Light)
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh
import SketchPlane3d
import SolidAngle
import Sphere3d exposing (Sphere3d)
import Task
import Temperature
import Triangle3d
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import Viewpoint3d
import Volume exposing (Volume)
import WebGL.Texture


type Object coordinates
    = ObjectGroup
        { subObjects : List (Object coordinates)
        , boundingBox : Collider coordinates
        , name : String
        }
    | Object
        { shape : Entity coordinates
        , boundingBox : Collider coordinates
        , name : String
        , meshHash : String
        , customMesh : CustomMesh coordinates
        , approxShape : ShapeApproximation coordinates
        , rotation : { pitch : Angle, roll : Angle, yaw : Angle }
        }


type Collider coordinates
    = None
    | Box (BoundingBox3d Meters coordinates)


type CustomMesh coordinates
    = Primitive
    | Ellipsoid Dimension
    | Ring Float Float
    | PolyCone PointList ( Float, Float, Float )
    | PolyCylinder PointList Float
    | TexturedCylinder Float Float
    | TruncatedCone Float Float Float
    | CustomObject
        (TriangularMesh
            { position : Point3d Meters coordinates
            , uv : ( Float, Float )
            }
        )
        Bool


type ShapeApproximation coordinates
    = EmptyShape
    | Block (Block3d Meters coordinates)
    | Sphere (Sphere3d Meters coordinates)
    | EllipsoidShape (Cylinder3d Meters coordinates) Dimension
    | RingShape (Cylinder3d Meters coordinates) (Cylinder3d Meters coordinates)
    | Cone (Cone3d Meters coordinates)
    | Cylinder (Cylinder3d Meters coordinates)


type alias Dimension =
    ( Float, Float, Float )


type alias PointList =
    List ( Float, Float )


type alias MeshStore coordinates =
    { generatedMeshes : Dict String (Mesh.Textured coordinates)
    , generatedShadows : Dict String (Mesh.Shadow coordinates)
    }


type alias Mold coordinates a =
    Material coordinates { a | normals : () } -> Object coordinates


type alias TexturedMold coordinates a =
    Material coordinates { a | normals : (), uvs : () } -> Object coordinates



-- Materials
-- "Material coordinates { a | normals : () }" allegedly allows us to not have to bother with textured vs. uniform


{-| Makes a shape look metallic. Takes a colour and roughness value.
-}
metallic : Color.Color -> Float -> Mold coordinates a -> Object coordinates
metallic colour roughness shapeFunc =
    Material.metal { baseColor = colour, roughness = roughness } |> shapeFunc


{-| Makes a shape look like plastic, wood, or other similar materials. Takes a colour and roughness value.
-}
plastic : Color.Color -> Float -> Mold coordinates a -> Object coordinates
plastic colour roughness shapeFunc =
    Material.nonmetal { baseColor = colour, roughness = roughness } |> shapeFunc


{-| Makes a shape nonreflective. Takes only a colour.
-}
matte : Color.Color -> Mold coordinates a -> Object coordinates
matte colour shapeFunc =
    Material.matte colour |> shapeFunc


{-| Makes a shape look as metallic as you want. Takes colour, roughness, and metallicity values.
-}
customMat : Color.Color -> Float -> Float -> Mold coordinates a -> Object coordinates
customMat colour roughness metallicity shapeFunc =
    Material.pbr { baseColor = colour, roughness = roughness, metallic = metallicity } |> shapeFunc


{-| Makes a shape look as metallic as you want, and applies a texture to it. Takes a texture, as well as roughness and metallicity values.
-}
textured : Material.Texture Color.Color -> Material.Texture Float -> Material.Texture Float -> TexturedMold coordinates a -> Object coordinates
textured texture roughness metallicity shapeFunc =
    Material.texturedPbr
        { baseColor = texture
        , roughness = roughness
        , metallic = metallicity
        }
        |> shapeFunc


{-| Returns a constant numerical texture for roughness or metallicity.
-}
constantTexture : Float -> Material.Texture Float
constantTexture val =
    Material.constant val


{-| Returns a constant colour texture for the colour channel.
-}
constantColourTexture : Color.Color -> Material.Texture Color.Color
constantColourTexture col =
    Material.constant col



--- Shapes


{-| Create an Object from an existing Scene3d Entity. Assumes that you are handling mesh caching.
-}
fromEntity : Dimension -> Entity coordinates -> Object coordinates
fromEntity ( length, width, height ) entity =
    Object
        { shape = entity
        , boundingBox =
            Box
                (BoundingBox3d.from
                    (Point3d.centimeters (length / 2) (width / 2) (height / 2))
                    (Point3d.centimeters (-length / 2) (-width / 2) (-height / 2))
                )
        , name = "customEntity"
        , meshHash = ""
        , customMesh = Primitive
        , approxShape = EmptyShape
        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
        }


{-| Create both a Light and an Entity (a bright glowing sphere) representing a
particular point light
-}
pointLight :
    { position : Point3d Meters coordinates
    , chromaticity : Chromaticity
    , intensity : LuminousFlux
    , castsShadows : Bool
    , showEntity : Bool
    }
    -> ( Light coordinates Bool, Entity coordinates )
pointLight properties =
    let
        -- Create a sphere to represent a light bulb
        lightsphere =
            Sphere3d.atPoint properties.position (Length.millimeters 100)

        -- Calculate the luminance of the sphere surface by dividing the given
        -- total luminous flux of the light by the surface area of the sphere
        -- and by the solid angle of a hemisphere (assuming that each point on
        -- the surface of the bulb emits light equally in all directions)...I
        -- am not 100% sure this is exactly correct =)
        sphereLuminance =
            properties.intensity
                |> Quantity.per (SolidAngle.spats 0.5)
                |> Quantity.per (Sphere3d.surfaceArea lightsphere)

        -- Create an emissive (glowing) material for the sphere
        sphereMaterial =
            Material.emissive properties.chromaticity sphereLuminance

        sphereEntity =
            if properties.showEntity then
                Scene3d.sphere sphereMaterial lightsphere

            else
                Scene3d.nothing

        lightData =
            { position = properties.position
            , chromaticity = properties.chromaticity
            , intensity = properties.intensity
            }
    in
    ( Light.point (Light.castsShadows properties.castsShadows) lightData
    , sphereEntity
    )


{-| Creates a cube with the given side length.
-}
cube : Float -> Material.Uniform coordinates -> Object coordinates
cube size material =
    cubeU (Length.centimeters size) material


{-| Creates a cube with the given side length in signed units.
-}
cubeU : Length -> Material.Uniform coordinates -> Object coordinates
cubeU size material =
    boxU size size size material


{-| Creates a square with the given side length that has no thickness.
-}
square3D : Float -> Material.Textured coordinates -> Object coordinates
square3D length material =
    let
        lenVal =
            Length.centimeters length
    in
    quad3D lenVal lenVal material


{-| Creates a rectangle with the given length and width that has no thickness.
-}
rectangle3D : Float -> Float -> Material.Textured coordinates -> Object coordinates
rectangle3D length width material =
    quad3D (Length.centimeters length) (Length.centimeters width) material


{-| Creates 2D quad with no thickness using signed units.
-}
quad3D : Length -> Length -> Material.Textured coordinates -> Object coordinates
quad3D length width material =
    let
        lValue =
            length |> Quantity.divideBy 2

        wValue =
            width |> Quantity.divideBy 2

        rectangleShape =
            Scene3d.quadWithShadow material
                (Point3d.xyz (lValue |> Quantity.negate) (wValue |> Quantity.negate) Quantity.zero)
                (Point3d.xyz (lValue |> Quantity.negate) wValue Quantity.zero)
                (Point3d.xyz lValue wValue Quantity.zero)
                (Point3d.xyz lValue (wValue |> Quantity.negate) Quantity.zero)

        rectangleBBox =
            BoundingBox3d.from
                (Point3d.xyz (lValue |> Quantity.negate) (wValue |> Quantity.negate) Quantity.zero)
                (Point3d.xyz lValue wValue Quantity.zero)
    in
    Object
        { shape = rectangleShape
        , boundingBox = Box rectangleBBox
        , name = "rectangle"
        , meshHash = ""
        , customMesh = Primitive
        , approxShape = Block (Block3d.centeredOn Frame3d.atOrigin ( length, width, Length.centimeters 0.1 ))
        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
        }


{-| Creates a rectangular prism, or "box", with the given length, width, and height.
-}
box : Float -> Float -> Float -> Material.Uniform coordinates -> Object coordinates
box length width height material =
    boxU
        (Length.centimeters length)
        (Length.centimeters width)
        (Length.centimeters height)
        material


{-| Creates a rectangular prism, or "box", with the given length, width, and height.
-}
boxU : Length -> Length -> Length -> Material.Uniform coordinates -> Object coordinates
boxU length width height material =
    let
        boxShape =
            Block3d.from
                (Point3d.xyz
                    (length
                        |> Quantity.divideBy -2
                    )
                    (width
                        |> Quantity.divideBy -2
                    )
                    (height
                        |> Quantity.divideBy -2
                    )
                )
                (Point3d.xyz
                    (length
                        |> Quantity.divideBy 2
                    )
                    (width
                        |> Quantity.divideBy 2
                    )
                    (height
                        |> Quantity.divideBy 2
                    )
                )
    in
    Object
        { shape = Scene3d.blockWithShadow material boxShape
        , boundingBox = Block3d.boundingBox boxShape |> Box
        , name = "box"
        , meshHash = ""
        , customMesh = Primitive
        , approxShape =
            Block boxShape
        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
        }


{-| Creates a sphere with the given radius.
-}
sphere : Float -> Material.Textured coordinates -> Object coordinates
sphere r material =
    sphereU (Length.centimeters r) material


{-| Creates a sphere with the given radius, using signed units.
-}
sphereU : Length -> Material.Textured coordinates -> Object coordinates
sphereU r material =
    let
        sphereShape =
            Sphere3d.withRadius r Point3d.origin
    in
    Object
        { shape = Scene3d.sphereWithShadow material sphereShape
        , boundingBox = Sphere3d.boundingBox sphereShape |> Box
        , name = "sphere"
        , meshHash = ""
        , customMesh = Primitive
        , approxShape = Sphere sphereShape
        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
        }


{-| Creates a cone with the given radius and height.
-}
cone : Float -> Float -> Material.Uniform coordinates -> Object coordinates
cone r h material =
    coneU (Length.centimeters r) (Length.centimeters h) material


{-| Creates a cone with the given radius and height using signed units.
-}
coneU : Length -> Length -> Material.Uniform coordinates -> Object coordinates
coneU r h material =
    let
        coneShape =
            Cone3d.along Axis3d.z
                { base = Quantity.zero
                , tip = h
                , radius = r
                }
    in
    Object
        { shape = Scene3d.coneWithShadow material coneShape
        , boundingBox = Cone3d.boundingBox coneShape |> Box
        , name = "cone"
        , meshHash = ""
        , customMesh = Primitive
        , approxShape = Cone coneShape
        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
        }


polyCone : List ( Float, Float ) -> ( Float, Float, Float ) -> MeshStore coordinates -> Material.Textured coordinates -> Object coordinates
polyCone points ( xtip, ytip, ztip ) model material =
    let
        meshName =
            getMeshName (PolyCone points ( xtip, ytip, ztip ))

        mesh =
            case Dict.get meshName model.generatedMeshes of
                Nothing ->
                    polyConeMesh points ( xtip, ytip, ztip )

                Just actualMesh ->
                    actualMesh

        shadow =
            case Dict.get meshName model.generatedShadows of
                Nothing ->
                    Mesh.shadow (polyConeMesh points ( xtip, ytip, ztip ))

                Just actualMesh ->
                    actualMesh

        -- bottom = customPolygon points material
        minX =
            List.minimum (List.map (\( x, _ ) -> x) points)

        maxX =
            List.maximum (List.map (\( x, _ ) -> x) points)

        minY =
            List.minimum (List.map (\( _, y ) -> y) points)

        maxY =
            List.maximum (List.map (\( _, y ) -> y) points)

        avgRadius =
            (((Maybe.withDefault 0 maxX - Maybe.withDefault 0 minX) |> abs) + ((Maybe.withDefault 0 maxY - Maybe.withDefault 0 minY) |> abs)) / 2
    in
    case ( minX, maxX ) of
        ( Just minx, Just maxx ) ->
            case ( minY, maxY ) of
                ( Just miny, Just maxy ) ->
                    Object
                        { shape = Scene3d.meshWithShadow material mesh shadow
                        , boundingBox =
                            BoundingBox3d.from
                                (Point3d.centimeters (maxx + xtip) (maxy + ytip) ztip)
                                (Point3d.centimeters minx miny 0)
                                |> Box
                        , name = "polyCone"
                        , meshHash = meshName
                        , customMesh = PolyCone points ( xtip, ytip, ztip )
                        , approxShape =
                            Cone
                                (Cone3d.along Axis3d.z
                                    { base = Quantity.zero
                                    , tip = ztip |> Length.centimeters
                                    , radius = avgRadius |> Length.centimeters
                                    }
                                )
                        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
                        }

                ( _, _ ) ->
                    Object
                        { shape = Scene3d.meshWithShadow material mesh shadow
                        , boundingBox = None
                        , name = "polyCone"
                        , meshHash = meshName
                        , customMesh = PolyCone points ( xtip, ytip, ztip )
                        , approxShape =
                            Cone
                                (Cone3d.along Axis3d.z
                                    { base = Quantity.zero
                                    , tip = ztip |> Length.centimeters
                                    , radius = avgRadius |> Length.centimeters
                                    }
                                )
                        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
                        }

        ( _, _ ) ->
            Object
                { shape = Scene3d.meshWithShadow material mesh shadow
                , boundingBox = None
                , name = "polyCone"
                , meshHash = meshName
                , customMesh = PolyCone points ( xtip, ytip, ztip )
                , approxShape =
                    Cone
                        (Cone3d.along Axis3d.z
                            { base = Quantity.zero
                            , tip = ztip |> Length.centimeters
                            , radius = avgRadius |> Length.centimeters
                            }
                        )
                , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
                }


{-| Create a truncated cone that you can apply textures to with the given radii and height.
Requires that you pass in the model as well, in order for it to retrieve the required meshes.
-}
truncatedCone : Float -> Float -> Float -> MeshStore coordinates -> Material.Textured coordinates -> Object coordinates
truncatedCone topR botR height model material =
    let
        meshName =
            getMeshName (TruncatedCone topR botR height)

        mesh =
            case Dict.get meshName model.generatedMeshes of
                Nothing ->
                    truncatedConeMesh topR botR height

                Just actualMesh ->
                    actualMesh

        shadow =
            case Dict.get meshName model.generatedShadows of
                Nothing ->
                    Mesh.shadow (truncatedConeMesh topR botR height)

                Just actualMesh ->
                    actualMesh

        largestRadius =
            if abs topR > abs botR then
                topR

            else
                botR

        avgRadius =
            (topR + botR) / 2

        boundingBox =
            BoundingBox3d.fromExtrema
                { minX = Length.centimeters (0 - largestRadius)
                , maxX = Length.centimeters (0 + largestRadius)
                , minY = Length.centimeters (0 - largestRadius)
                , maxY = Length.centimeters (0 + largestRadius)
                , minZ = Length.centimeters 0
                , maxZ = Length.centimeters (0 + height)
                }
    in
    Object
        { shape = Scene3d.meshWithShadow material mesh shadow
        , boundingBox = Box boundingBox
        , name = "truncatedCone"
        , meshHash = meshName
        , customMesh = TruncatedCone topR botR height
        , approxShape =
            Cylinder
                (Cylinder3d.along Axis3d.z
                    { start = Quantity.zero
                    , end = height |> Length.centimeters
                    , radius = avgRadius |> Length.centimeters
                    }
                )
        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
        }


polyCylinder : List ( Float, Float ) -> Float -> MeshStore coordinates -> Material.Textured coordinates -> Object coordinates
polyCylinder points height model material =
    let
        meshName =
            getMeshName (PolyCylinder points height)

        -- bottom =  customPolygon points material
        -- top = customPolygon points material
        --            |> move3D (0,0,height)
        mesh =
            case Dict.get meshName model.generatedMeshes of
                Nothing ->
                    polyCylinderMesh points height

                Just actualMesh ->
                    actualMesh

        shadow =
            case Dict.get meshName model.generatedShadows of
                Nothing ->
                    Mesh.shadow (polyCylinderMesh points height)

                Just actualMesh ->
                    actualMesh

        minX =
            List.minimum (List.map (\( x, _ ) -> x) points)

        maxX =
            List.maximum (List.map (\( x, _ ) -> x) points)

        minY =
            List.minimum (List.map (\( _, y ) -> y) points)

        maxY =
            List.maximum (List.map (\( _, y ) -> y) points)

        avgRadius =
            (((Maybe.withDefault 0 maxX - Maybe.withDefault 0 minX) |> abs) + ((Maybe.withDefault 0 maxY - Maybe.withDefault 0 minY) |> abs)) / 2
    in
    case ( minX, maxX ) of
        ( Just minx, Just maxx ) ->
            case ( minY, maxY ) of
                ( Just miny, Just maxy ) ->
                    Object
                        { shape = Scene3d.meshWithShadow material mesh shadow
                        , boundingBox =
                            BoundingBox3d.from
                                (Point3d.centimeters maxx maxy height)
                                (Point3d.centimeters minx miny 0)
                                |> Box
                        , name = "polyCylinder"
                        , meshHash = meshName
                        , customMesh = PolyCylinder points height
                        , approxShape =
                            Cylinder
                                (Cylinder3d.along Axis3d.z
                                    { start = Quantity.zero
                                    , end = height |> Length.centimeters
                                    , radius = avgRadius |> Length.centimeters
                                    }
                                )
                        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
                        }

                ( _, _ ) ->
                    Object
                        { shape = Scene3d.meshWithShadow material mesh shadow
                        , boundingBox = None
                        , name = "polyCylinder"
                        , meshHash = meshName
                        , customMesh = PolyCylinder points height
                        , approxShape =
                            Cylinder
                                (Cylinder3d.along Axis3d.z
                                    { start = Quantity.zero
                                    , end = height |> Length.centimeters
                                    , radius = avgRadius |> Length.centimeters
                                    }
                                )
                        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
                        }

        ( _, _ ) ->
            Object
                { shape = Scene3d.meshWithShadow material mesh shadow
                , boundingBox = None
                , name = "polyCylinder"
                , meshHash = meshName
                , customMesh = PolyCylinder points height
                , approxShape =
                    Cylinder
                        (Cylinder3d.along Axis3d.z
                            { start = Quantity.zero
                            , end = height |> Length.centimeters
                            , radius = avgRadius |> Length.centimeters
                            }
                        )
                , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
                }


{-| Creates a cylinder with the given radius and height.
-}
cylinder : Float -> Float -> Material.Uniform coordinates -> Object coordinates
cylinder r h material =
    cylinderU (Length.centimeters r) (Length.centimeters h) material


{-| Creates a cylinder with the given radius and height, using signed units.
-}
cylinderU : Length -> Length -> Material.Uniform coordinates -> Object coordinates
cylinderU r h material =
    let
        cylinderShape =
            Cylinder3d.centeredOn
                Point3d.origin
                Direction3d.z
                { length = h
                , radius = r
                }
    in
    Object
        { shape = Scene3d.cylinderWithShadow material cylinderShape
        , boundingBox = Cylinder3d.boundingBox cylinderShape |> Box
        , name = "cylinder"
        , meshHash = ""
        , customMesh = Primitive
        , approxShape = Cylinder cylinderShape
        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
        }


cylinderStartingAt :
    Point3d Meters coordinates
    -> Direction3d coordinates
    -> { radius : Quantity Float Meters, length : Quantity Float Meters }
    -> Material.Uniform coordinates
    -> Object coordinates
cylinderStartingAt pt dir properties material =
    let
        cylinderShape =
            Cylinder3d.startingAt pt dir properties
    in
    Object
        { shape = Scene3d.cylinderWithShadow material cylinderShape
        , boundingBox = Cylinder3d.boundingBox cylinderShape |> Box
        , name = "cylinder"
        , meshHash = ""
        , customMesh = Primitive
        , approxShape = Cylinder cylinderShape
        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
        }


coneStartingAt :
    Point3d Meters coordinates
    -> Direction3d coordinates
    -> { radius : Quantity Float Meters, length : Quantity Float Meters }
    -> Material.Uniform coordinates
    -> Object coordinates
coneStartingAt pt dir properties material =
    let
        coneShape =
            Cone3d.startingAt pt dir properties
    in
    Object
        { shape = Scene3d.coneWithShadow material coneShape
        , boundingBox = Cone3d.boundingBox coneShape |> Box
        , name = "cylinder"
        , meshHash = ""
        , customMesh = Primitive
        , approxShape = Cone coneShape
        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
        }


arrowStartingAt :
    Point3d Meters coordinates
    -> Direction3d coordinates
    -> { radius : Quantity Float Meters, length : Quantity Float Meters }
    -> Material.Uniform coordinates
    -> Object coordinates
arrowStartingAt pt dir properties material =
    let
        coneLength =
            properties.radius
                |> Quantity.multiplyBy 2

        cylinderLength =
            properties.length
                |> Quantity.minus coneLength

        cylinderShape =
            cylinderStartingAt pt
                dir
                { radius = properties.radius
                , length = cylinderLength
                }

        coneStartingPt =
            pt
                |> Point3d.translateIn
                    dir
                    cylinderLength

        coneShape =
            coneStartingAt coneStartingPt dir { radius = properties.radius |> Quantity.multiplyBy 1.5, length = coneLength }
    in
    group3D
        [ cylinderShape
            material
        , coneShape
            material
        ]


{-| Create a cylinder that you can apply textures to with the given radius and height.
Requires that you pass in the model as well, in order for it to retrieve the required meshes.
-}
texturedCylinder : Float -> Float -> MeshStore coordinates -> Material.Textured coordinates -> Object coordinates
texturedCylinder radius height model material =
    let
        meshName =
            getMeshName (TexturedCylinder radius height)

        mesh =
            case Dict.get meshName model.generatedMeshes of
                Nothing ->
                    texturedCylinderMesh radius height

                Just actualMesh ->
                    actualMesh

        shadow =
            case Dict.get meshName model.generatedShadows of
                Nothing ->
                    Mesh.shadow (texturedCylinderMesh radius height)

                Just actualMesh ->
                    actualMesh

        boundingBox =
            BoundingBox3d.fromExtrema
                { minX = Length.centimeters (0 - radius)
                , maxX = Length.centimeters (0 + radius)
                , minY = Length.centimeters (0 - radius)
                , maxY = Length.centimeters (0 + radius)
                , minZ = Length.centimeters 0
                , maxZ = Length.centimeters (0 + height)
                }
    in
    Object
        { shape = Scene3d.meshWithShadow material mesh shadow
        , boundingBox = Box boundingBox
        , name = "texturedCylinder"
        , meshHash = meshName
        , customMesh = TexturedCylinder radius height
        , approxShape =
            Cylinder
                (Cylinder3d.along Axis3d.z
                    { start = Quantity.zero
                    , end = height |> Length.centimeters
                    , radius = radius |> Length.centimeters
                    }
                )
        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
        }


ring : Float -> Float -> MeshStore coordinates -> Material.Textured coordinates -> Object coordinates
ring radius thickness model material =
    let
        meshName =
            getMeshName (Ring radius thickness)

        mesh =
            case Dict.get meshName model.generatedMeshes of
                Nothing ->
                    ringMesh radius thickness

                Just actualMesh ->
                    actualMesh

        shadow =
            case Dict.get meshName model.generatedShadows of
                Nothing ->
                    Mesh.shadow (ringMesh radius thickness)

                Just actualMesh ->
                    actualMesh

        outerCylinder =
            Cylinder3d.along Axis3d.z
                { start = -thickness |> Length.centimeters
                , end = thickness |> Length.centimeters
                , radius = (radius + thickness) |> Length.centimeters
                }

        innerCylinder =
            Cylinder3d.along Axis3d.z
                { start = -thickness |> Length.centimeters
                , end = thickness |> Length.centimeters
                , radius = (radius - thickness) |> Length.centimeters
                }
    in
    Object
        { shape = Scene3d.meshWithShadow material mesh shadow
        , boundingBox =
            --Box (BoundingBox3d.aggregate forwardBBox [ backwardBBox, leftBBox, rightBBox ])
            Box (outerCylinder |> Cylinder3d.boundingBox)
        , name = "ring"
        , meshHash = meshName
        , customMesh = Ring radius thickness
        , approxShape =
            RingShape outerCylinder innerCylinder
        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
        }


{-| Create an ellipsoid with a custom length, width, and height.
Requires that you pass in the model as well, in order for it to retrieve the required meshes.
-}
ellipsoid : Float -> Float -> Float -> MeshStore coordinates -> Material.Textured coordinates -> Object coordinates
ellipsoid length width height model material =
    let
        meshName =
            getMeshName (Ellipsoid ( length, width, height ))

        mesh =
            case Dict.get meshName model.generatedMeshes of
                Nothing ->
                    ellipsoidMesh length width height

                Just actualMesh ->
                    actualMesh

        shadow =
            case Dict.get meshName model.generatedShadows of
                Nothing ->
                    Mesh.shadow (ellipsoidMesh length width height)

                Just actualMesh ->
                    actualMesh

        ellipsoidBBox =
            BoundingBox3d.fromExtrema
                { minX = Length.centimeters (0 - length)
                , maxX = Length.centimeters (0 + length)
                , minY = Length.centimeters (0 - width)
                , maxY = Length.centimeters (0 + width)
                , minZ = Length.centimeters (0 - height)
                , maxZ = Length.centimeters (0 + height)
                }

        approxShape =
            if length > width && length > height then
                Cylinder3d.along Axis3d.x
                    { start = Length.centimeters (0 - length)
                    , end = Length.centimeters (0 + length)
                    , radius = Length.centimeters (max (2 * width) (2 * height))
                    }

            else if width > length && width > height then
                Cylinder3d.along Axis3d.y
                    { start = Length.centimeters (0 - width)
                    , end = Length.centimeters (0 + width)
                    , radius = Length.centimeters (max (2 * length) (2 * height))
                    }

            else
                Cylinder3d.along Axis3d.z
                    { start = Length.centimeters (0 - height)
                    , end = Length.centimeters (0 + height)
                    , radius = Length.centimeters (max (2 * width) (2 * length))
                    }
    in
    Object
        { shape = Scene3d.meshWithShadow material mesh shadow
        , boundingBox = Box ellipsoidBBox
        , name = "ellipsoid"
        , meshHash = meshName
        , customMesh = Ellipsoid ( length, width, height )
        , approxShape = EllipsoidShape approxShape ( length, width, height )
        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
        }


{-| Create a polygon with a custom number of sides (min. 3) and size, in centimeters.
-}
polygon3D : Int -> Float -> Material.Textured coordinates -> Object coordinates
polygon3D sides size material =
    let
        mesh =
            polygonMesh sides

        shadow =
            Mesh.shadow (polygonMesh sides)

        polygonBBox =
            BoundingBox3d.fromExtrema
                { minX = Length.centimeters (0 - 1)
                , maxX = Length.centimeters (0 + 1)
                , minY = Length.centimeters (0 - 1)
                , maxY = Length.centimeters (0 + 1)
                , minZ = Length.centimeters 0
                , maxZ = Length.centimeters 0
                }
    in
    Object
        { shape = Scene3d.meshWithShadow material mesh shadow
        , boundingBox = Box polygonBBox
        , name = "polygon"
        , meshHash = ""
        , customMesh = Primitive -- This actually gets dynamically generated, but right now the performance impact is negligible
        , approxShape =
            Block
                (Block3d.centeredOn Frame3d.atOrigin
                    ( Length.centimeters 2
                    , Length.centimeters 2
                    , Length.centimeters 0.1
                    )
                )

        -- Side length is 2cm, and scale3D will scale this for us
        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
        }
        |> scale3D size


{-| Create a custom polygon from a list of points
-}
customPolygon : List ( Float, Float ) -> Material.Textured coordinates -> Object coordinates
customPolygon points material =
    let
        mesh =
            customPolyMesh points

        shadow =
            \maxx minx maxy miny ->
                Mesh.shadow (customPolyMesh points maxx minx maxy miny)

        minX =
            List.minimum (List.map (\( x, _ ) -> x) points)

        maxX =
            List.maximum (List.map (\( x, _ ) -> x) points)

        minY =
            List.minimum (List.map (\( _, y ) -> y) points)

        maxY =
            List.maximum (List.map (\( _, y ) -> y) points)

        length =
            (((Maybe.withDefault 0 maxX + Maybe.withDefault 0 minX) |> abs) + ((Maybe.withDefault 0 maxY + Maybe.withDefault 0 minY) |> abs)) / 2
    in
    case ( minX, maxX ) of
        ( Just minx, Just maxx ) ->
            case ( minY, maxY ) of
                ( Just miny, Just maxy ) ->
                    Object
                        { shape =
                            Scene3d.meshWithShadow material
                                (mesh maxx minx maxy miny)
                                (shadow maxx minx maxy miny)
                        , boundingBox =
                            BoundingBox3d.from
                                (Point3d.centimeters maxx maxy 0)
                                (Point3d.centimeters minx miny 0)
                                |> Box
                        , name = "customPolygon"
                        , meshHash = ""
                        , customMesh = Primitive
                        , approxShape =
                            Block
                                (Block3d.centeredOn Frame3d.atOrigin
                                    ( Length.centimeters length
                                    , Length.centimeters length
                                    , Length.centimeters 0.1
                                    )
                                )
                        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
                        }

                ( _, _ ) ->
                    Object
                        { shape = Scene3d.meshWithShadow material (mesh 0 0 0 0) (shadow 0 0 0 0)
                        , boundingBox = None
                        , name = "customPolygon"
                        , meshHash = ""
                        , customMesh = Primitive
                        , approxShape =
                            Block
                                (Block3d.centeredOn Frame3d.atOrigin
                                    ( Length.centimeters length
                                    , Length.centimeters length
                                    , Length.centimeters 0.1
                                    )
                                )
                        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
                        }

        ( _, _ ) ->
            Object
                { shape = Scene3d.meshWithShadow material (mesh 0 0 0 0) (shadow 0 0 0 0)
                , boundingBox = None
                , name = "customPolygon"
                , meshHash = ""
                , customMesh = Primitive
                , approxShape =
                    Block
                        (Block3d.centeredOn Frame3d.atOrigin
                            ( Length.centimeters length
                            , Length.centimeters length
                            , Length.centimeters 0.1
                            )
                        )
                , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
                }


customObject :
    TriangularMesh { position : Point3d Meters coordinates, uv : ( Float, Float ) }
    -> Bool
    -> MeshStore coordinates
    -> Material.Textured coordinates
    -> Object coordinates
customObject triangularMesh doCulling model material =
    let
        meshName =
            getMeshName (CustomObject triangularMesh doCulling)

        mesh =
            case Dict.get meshName model.generatedMeshes of
                Nothing ->
                    dynamicMesh triangularMesh doCulling

                Just actualMesh ->
                    actualMesh

        shadow =
            case Dict.get meshName model.generatedShadows of
                Nothing ->
                    Mesh.shadow (dynamicMesh triangularMesh doCulling)

                Just actualMesh ->
                    actualMesh

        positions =
            TriangularMesh.vertices triangularMesh
                |> Array.toList
                |> List.map (\vertex -> vertex.position)

        minX =
            positions
                |> List.map Point3d.xCoordinate
                |> List.map Length.inCentimeters
                |> List.minimum
                |> Maybe.withDefault 0

        maxX =
            positions
                |> List.map Point3d.xCoordinate
                |> List.map Length.inCentimeters
                |> List.maximum
                |> Maybe.withDefault 0

        minY =
            positions
                |> List.map Point3d.yCoordinate
                |> List.map Length.inCentimeters
                |> List.minimum
                |> Maybe.withDefault 0

        maxY =
            positions
                |> List.map Point3d.yCoordinate
                |> List.map Length.inCentimeters
                |> List.maximum
                |> Maybe.withDefault 0

        minZ =
            positions
                |> List.map Point3d.zCoordinate
                |> List.map Length.inCentimeters
                |> List.minimum
                |> Maybe.withDefault 0

        maxZ =
            positions
                |> List.map Point3d.zCoordinate
                |> List.map Length.inCentimeters
                |> List.maximum
                |> Maybe.withDefault 0

        boundingBox =
            BoundingBox3d.from
                (Point3d.centimeters minX minY minZ)
                (Point3d.centimeters maxX maxY maxZ)
    in
    Object
        { shape = Scene3d.meshWithShadow material mesh shadow
        , boundingBox = Box boundingBox
        , name = "customObject"
        , meshHash = meshName
        , customMesh = CustomObject triangularMesh doCulling
        , approxShape = Block (boundingBox |> Block3d.fromBoundingBox)
        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
        }



--Create a box outline for showing the bounding box


boxOutline : Material.Plain coordinates -> BoundingBox3d Meters coordinates -> Entity coordinates
boxOutline material boundingBox =
    let
        toBlock3d =
            Block3d.with
                { x1 = BoundingBox3d.minX boundingBox
                , y1 = BoundingBox3d.minY boundingBox
                , z1 = BoundingBox3d.minZ boundingBox
                , x2 = BoundingBox3d.maxX boundingBox
                , y2 = BoundingBox3d.maxY boundingBox
                , z2 = BoundingBox3d.maxZ boundingBox
                }

        edges =
            Block3d.edges toBlock3d
    in
    Scene3d.group
        (List.map
            (\line ->
                Scene3d.lineSegment material line
            )
            edges
        )



-- Transformations


move3DU : Point3d Meters coordinates -> Object coordinates -> Object coordinates
move3DU pt object =
    let
        tup =
            Point3d.toTuple Length.inCentimeters pt
    in
    move3D tup object


{-| Translates an object by the given 3D coordinates
-}
move3D : Dimension -> Object coordinates -> Object coordinates
move3D ( x, y, z ) object =
    let
        displacement =
            Vector3d.centimeters x y z
    in
    case object of
        ObjectGroup attributes ->
            ObjectGroup
                { attributes
                    | subObjects =
                        List.map (\subObject -> move3D ( x, y, z ) subObject) attributes.subObjects
                    , boundingBox =
                        case attributes.boundingBox of
                            Box boundingbox ->
                                boundingbox
                                    |> BoundingBox3d.translateBy displacement
                                    |> Box

                            None ->
                                None
                }

        Object attributes ->
            case attributes.boundingBox of
                Box boundingBox ->
                    Object
                        { attributes
                            | shape =
                                attributes.shape
                                    |> Scene3d.translateBy displacement
                            , boundingBox =
                                boundingBox
                                    |> BoundingBox3d.translateBy displacement
                                    |> Box
                            , approxShape =
                                case attributes.approxShape of
                                    EmptyShape ->
                                        EmptyShape

                                    Block shape ->
                                        shape
                                            |> Block3d.translateBy displacement
                                            |> Block

                                    Sphere shape ->
                                        shape
                                            |> Sphere3d.translateBy displacement
                                            |> Sphere

                                    EllipsoidShape shape dimensions ->
                                        EllipsoidShape
                                            (shape
                                                |> Cylinder3d.translateBy displacement
                                            )
                                            dimensions

                                    RingShape outer inner ->
                                        RingShape
                                            (outer |> Cylinder3d.translateBy displacement)
                                            (inner |> Cylinder3d.translateBy displacement)

                                    Cone shape ->
                                        shape
                                            |> Cone3d.translateBy displacement
                                            |> Cone

                                    Cylinder shape ->
                                        shape
                                            |> Cylinder3d.translateBy displacement
                                            |> Cylinder
                        }

                None ->
                    Object
                        { attributes
                            | shape = attributes.shape |> Scene3d.translateBy (Vector3d.centimeters x y z)
                            , boundingBox = None
                            , approxShape =
                                case attributes.approxShape of
                                    EmptyShape ->
                                        EmptyShape

                                    Block shape ->
                                        shape
                                            |> Block3d.translateBy displacement
                                            |> Block

                                    Sphere shape ->
                                        shape
                                            |> Sphere3d.translateBy displacement
                                            |> Sphere

                                    EllipsoidShape shape dimensions ->
                                        EllipsoidShape
                                            (shape
                                                |> Cylinder3d.translateBy displacement
                                            )
                                            dimensions

                                    RingShape outer inner ->
                                        RingShape
                                            (outer |> Cylinder3d.translateBy displacement)
                                            (inner |> Cylinder3d.translateBy displacement)

                                    Cone shape ->
                                        shape
                                            |> Cone3d.translateBy displacement
                                            |> Cone

                                    Cylinder shape ->
                                        shape
                                            |> Cylinder3d.translateBy displacement
                                            |> Cylinder
                        }


rotate3DU : Angle -> Angle -> Angle -> Object coordinates -> Object coordinates
rotate3DU pitch yaw roll =
    rotate3D (Angle.inRadians pitch) (Angle.inRadians yaw) (Angle.inRadians roll)


{-| Rotates an object around all three axes.
-}
rotate3D : Float -> Float -> Float -> Object coordinates -> Object coordinates
rotate3D pitch yaw roll object =
    let
        toBlock3d boundingBox =
            Block3d.with
                { x1 = BoundingBox3d.minX boundingBox
                , y1 = BoundingBox3d.minY boundingBox
                , z1 = BoundingBox3d.minZ boundingBox
                , x2 = BoundingBox3d.maxX boundingBox
                , y2 = BoundingBox3d.maxY boundingBox
                , z2 = BoundingBox3d.maxZ boundingBox
                }
    in
    case object of
        ObjectGroup attributes ->
            ObjectGroup
                { attributes
                    | subObjects = List.map (\subObject -> rotate3D pitch yaw roll subObject) attributes.subObjects
                    , boundingBox =
                        case attributes.boundingBox of
                            Box boundingbox ->
                                toBlock3d boundingbox
                                    |> Block3d.rotateAround Axis3d.y (Angle.radians pitch)
                                    |> Block3d.rotateAround Axis3d.x (Angle.radians roll)
                                    |> Block3d.rotateAround Axis3d.z (Angle.radians yaw)
                                    |> Block3d.boundingBox
                                    |> Box

                            None ->
                                None
                }

        Object attributes ->
            case attributes.boundingBox of
                Box boundingBox ->
                    Object
                        { attributes
                            | shape =
                                attributes.shape
                                    |> Scene3d.rotateAround Axis3d.y (Angle.radians pitch)
                                    |> Scene3d.rotateAround Axis3d.x (Angle.radians roll)
                                    |> Scene3d.rotateAround Axis3d.z (Angle.radians yaw)
                            , boundingBox =
                                toBlock3d boundingBox
                                    |> Block3d.rotateAround Axis3d.y (Angle.radians pitch)
                                    |> Block3d.rotateAround Axis3d.x (Angle.radians roll)
                                    |> Block3d.rotateAround Axis3d.z (Angle.radians yaw)
                                    |> Block3d.boundingBox
                                    |> Box
                            , rotation =
                                { pitch = attributes.rotation.pitch |> Quantity.plus (Angle.radians pitch)
                                , roll = attributes.rotation.roll |> Quantity.plus (Angle.radians roll)
                                , yaw = attributes.rotation.yaw |> Quantity.plus (Angle.radians yaw)
                                }
                            , approxShape =
                                case attributes.approxShape of
                                    EmptyShape ->
                                        EmptyShape

                                    Block shape ->
                                        Block
                                            (shape
                                                |> Block3d.rotateAround Axis3d.y (Angle.radians pitch)
                                                |> Block3d.rotateAround Axis3d.x (Angle.radians roll)
                                                |> Block3d.rotateAround Axis3d.z (Angle.radians yaw)
                                            )

                                    Sphere shape ->
                                        Sphere
                                            (shape
                                                |> Sphere3d.rotateAround Axis3d.y (Angle.radians pitch)
                                                |> Sphere3d.rotateAround Axis3d.x (Angle.radians roll)
                                                |> Sphere3d.rotateAround Axis3d.z (Angle.radians yaw)
                                            )

                                    EllipsoidShape shape dimensions ->
                                        EllipsoidShape
                                            (shape
                                                |> Cylinder3d.rotateAround Axis3d.y (Angle.radians pitch)
                                                |> Cylinder3d.rotateAround Axis3d.x (Angle.radians roll)
                                                |> Cylinder3d.rotateAround Axis3d.z (Angle.radians yaw)
                                            )
                                            dimensions

                                    RingShape outer inner ->
                                        RingShape
                                            (outer
                                                |> Cylinder3d.rotateAround Axis3d.y (Angle.radians pitch)
                                                |> Cylinder3d.rotateAround Axis3d.x (Angle.radians roll)
                                                |> Cylinder3d.rotateAround Axis3d.z (Angle.radians yaw)
                                            )
                                            (inner
                                                |> Cylinder3d.rotateAround Axis3d.y (Angle.radians pitch)
                                                |> Cylinder3d.rotateAround Axis3d.x (Angle.radians roll)
                                                |> Cylinder3d.rotateAround Axis3d.z (Angle.radians yaw)
                                            )

                                    Cone shape ->
                                        Cone
                                            (shape
                                                |> Cone3d.rotateAround Axis3d.y (Angle.radians pitch)
                                                |> Cone3d.rotateAround Axis3d.x (Angle.radians roll)
                                                |> Cone3d.rotateAround Axis3d.z (Angle.radians yaw)
                                            )

                                    Cylinder shape ->
                                        Cylinder
                                            (shape
                                                |> Cylinder3d.rotateAround Axis3d.y (Angle.radians pitch)
                                                |> Cylinder3d.rotateAround Axis3d.x (Angle.radians roll)
                                                |> Cylinder3d.rotateAround Axis3d.z (Angle.radians yaw)
                                            )
                        }

                None ->
                    Object
                        { attributes
                            | shape =
                                attributes.shape
                                    |> Scene3d.rotateAround Axis3d.y (Angle.radians pitch)
                                    |> Scene3d.rotateAround Axis3d.x (Angle.radians roll)
                                    |> Scene3d.rotateAround Axis3d.z (Angle.radians yaw)
                            , boundingBox = None
                            , rotation =
                                { pitch = attributes.rotation.pitch |> Quantity.plus (Angle.radians pitch)
                                , roll = attributes.rotation.roll |> Quantity.plus (Angle.radians roll)
                                , yaw = attributes.rotation.yaw |> Quantity.plus (Angle.radians yaw)
                                }
                            , approxShape =
                                case attributes.approxShape of
                                    EmptyShape ->
                                        EmptyShape

                                    Block shape ->
                                        Block
                                            (shape
                                                |> Block3d.rotateAround Axis3d.y (Angle.radians pitch)
                                                |> Block3d.rotateAround Axis3d.x (Angle.radians roll)
                                                |> Block3d.rotateAround Axis3d.z (Angle.radians yaw)
                                            )

                                    Sphere shape ->
                                        Sphere
                                            (shape
                                                |> Sphere3d.rotateAround Axis3d.y (Angle.radians pitch)
                                                |> Sphere3d.rotateAround Axis3d.x (Angle.radians roll)
                                                |> Sphere3d.rotateAround Axis3d.z (Angle.radians yaw)
                                            )

                                    EllipsoidShape shape dimensions ->
                                        EllipsoidShape
                                            (shape
                                                |> Cylinder3d.rotateAround Axis3d.y (Angle.radians pitch)
                                                |> Cylinder3d.rotateAround Axis3d.x (Angle.radians roll)
                                                |> Cylinder3d.rotateAround Axis3d.z (Angle.radians yaw)
                                            )
                                            dimensions

                                    RingShape outer inner ->
                                        RingShape
                                            (outer
                                                |> Cylinder3d.rotateAround Axis3d.y (Angle.radians pitch)
                                                |> Cylinder3d.rotateAround Axis3d.x (Angle.radians roll)
                                                |> Cylinder3d.rotateAround Axis3d.z (Angle.radians yaw)
                                            )
                                            (inner
                                                |> Cylinder3d.rotateAround Axis3d.y (Angle.radians pitch)
                                                |> Cylinder3d.rotateAround Axis3d.x (Angle.radians roll)
                                                |> Cylinder3d.rotateAround Axis3d.z (Angle.radians yaw)
                                            )

                                    Cone shape ->
                                        Cone
                                            (shape
                                                |> Cone3d.rotateAround Axis3d.y (Angle.radians pitch)
                                                |> Cone3d.rotateAround Axis3d.x (Angle.radians roll)
                                                |> Cone3d.rotateAround Axis3d.z (Angle.radians yaw)
                                            )

                                    Cylinder shape ->
                                        Cylinder
                                            (shape
                                                |> Cylinder3d.rotateAround Axis3d.y (Angle.radians pitch)
                                                |> Cylinder3d.rotateAround Axis3d.x (Angle.radians roll)
                                                |> Cylinder3d.rotateAround Axis3d.z (Angle.radians yaw)
                                            )
                        }



-- Single-axis rotation functions


{-| Rotate an object around the X axis by the given angle in radians
-}
rotateX3D : Float -> Object coordinates -> Object coordinates
rotateX3D angle object =
    object |> rotate3D 0 0 angle


{-| Rotate an object around the Y axis by the given angle in radians
-}
rotateY3D : Float -> Object coordinates -> Object coordinates
rotateY3D angle object =
    object |> rotate3D angle 0 0


{-| Rotate an object around the Z axis by the given angle in radians
-}
rotateZ3D : Float -> Object coordinates -> Object coordinates
rotateZ3D angle object =
    object |> rotate3D 0 angle 0


placeObject :
    Frame3d.Frame3d Meters coordinates { defines : localCoordinates }
    -> Object localCoordinates
    -> Object coordinates
placeObject frame obj =
    case obj of
        ObjectGroup attributes ->
            ObjectGroup
                { subObjects = List.map (placeObject frame) attributes.subObjects
                , boundingBox = None
                , name = attributes.name
                }

        Object attributes ->
            Object
                { shape = Scene3d.placeIn frame attributes.shape
                , boundingBox = None
                , customMesh =
                    case attributes.customMesh of
                        Primitive ->
                            Primitive

                        Ellipsoid dimensions ->
                            Ellipsoid dimensions

                        Ring r t ->
                            Ring r t

                        PolyCone points tipPos ->
                            PolyCone points tipPos

                        PolyCylinder points h ->
                            PolyCylinder points h

                        TexturedCylinder r h ->
                            TexturedCylinder r h

                        TruncatedCone rt rb h ->
                            TruncatedCone rt rb h

                        CustomObject triangularMesh doCulling ->
                            CustomObject
                                (triangularMesh
                                    |> TriangularMesh.mapVertices
                                        (\vertex -> { position = vertex.position |> Point3d.placeIn Frame3d.atOrigin, uv = vertex.uv })
                                )
                                doCulling
                , name = attributes.name
                , meshHash = getMeshName attributes.customMesh
                , approxShape =
                    case attributes.approxShape of
                        EmptyShape ->
                            EmptyShape

                        Block shape ->
                            Block (shape |> Block3d.placeIn frame)

                        Sphere shape ->
                            Sphere (shape |> Sphere3d.placeIn frame)

                        EllipsoidShape shape dimensions ->
                            EllipsoidShape (shape |> Cylinder3d.placeIn frame) dimensions

                        RingShape outer inner ->
                            RingShape
                                (outer |> Cylinder3d.placeIn frame)
                                (inner |> Cylinder3d.placeIn frame)

                        Cone shape ->
                            Cone (shape |> Cone3d.placeIn frame)

                        Cylinder shape ->
                            Cylinder (shape |> Cylinder3d.placeIn frame)
                , rotation = attributes.rotation
                }


{-| Scales an object by the given factor
-}
scale3D : Float -> Object coordinates -> Object coordinates
scale3D factor object =
    case object of
        ObjectGroup attributes ->
            ObjectGroup
                { attributes
                    | subObjects = List.map (\subObject -> scale3D factor subObject) attributes.subObjects
                    , boundingBox =
                        case attributes.boundingBox of
                            Box boundingbox ->
                                boundingbox
                                    |> BoundingBox3d.scaleAbout Point3d.origin factor
                                    |> Box

                            None ->
                                None
                }

        Object attributes ->
            let
                scaledShape =
                    case attributes.approxShape of
                        EmptyShape ->
                            EmptyShape

                        Block shape ->
                            Block (shape |> Block3d.scaleAbout Point3d.origin factor)

                        Sphere shape ->
                            Sphere (shape |> Sphere3d.scaleAbout Point3d.origin factor)

                        EllipsoidShape shape ( l, w, h ) ->
                            EllipsoidShape (shape |> Cylinder3d.scaleAbout Point3d.origin factor) ( l * factor, w * factor, h * factor )

                        RingShape outer inner ->
                            RingShape
                                (outer |> Cylinder3d.scaleAbout Point3d.origin factor)
                                (inner |> Cylinder3d.scaleAbout Point3d.origin factor)

                        Cone shape ->
                            Cone (shape |> Cone3d.scaleAbout Point3d.origin factor)

                        Cylinder shape ->
                            Cylinder (shape |> Cylinder3d.scaleAbout Point3d.origin factor)
            in
            case attributes.boundingBox of
                Box boundingBox ->
                    Object
                        { attributes
                            | shape =
                                attributes.shape
                                    |> Scene3d.scaleAbout Point3d.origin factor
                            , boundingBox =
                                boundingBox
                                    |> BoundingBox3d.scaleAbout Point3d.origin factor
                                    |> Box
                            , approxShape = scaledShape
                        }

                None ->
                    Object
                        { attributes
                            | shape =
                                attributes.shape
                                    |> Scene3d.scaleAbout Point3d.origin factor
                            , boundingBox = attributes.boundingBox
                            , approxShape = scaledShape
                        }


{-| ScaleTo3D scales an object to the giving size based on the giving dirextion
-}
scaleTo3D : Float -> Direction3d coordinates -> Object coordinates -> Object coordinates
scaleTo3D scaleTo direction object =
    let
        boxSize box_attri =
            let
                boxExtrema =
                    BoundingBox3d.extrema box_attri
            in
            if direction == Direction3d.x then
                Length.inCentimeters <|
                    Length.meters <|
                        Quantity.in_ Length.meters <|
                            Quantity.minus boxExtrema.maxX boxExtrema.minX

            else if direction == Direction3d.y then
                Length.inCentimeters <|
                    Length.meters <|
                        Quantity.in_ Length.meters <|
                            Quantity.minus boxExtrema.maxY boxExtrema.minY

            else
                Length.inCentimeters <|
                    Length.meters <|
                        Quantity.in_ Length.meters <|
                            Quantity.minus boxExtrema.maxZ boxExtrema.minZ

        scale to boxsize =
            to / boxsize

        aggregate boxList =
            case BoundingBox3d.aggregateN boxList of
                Just attri ->
                    attri

                Nothing ->
                    BoundingBox3d.singleton Point3d.origin
    in
    case object of
        ObjectGroup attri ->
            case attri.boundingBox of
                Box box_attri ->
                    object |> scale3D (scale scaleTo (boxSize box_attri))

                None ->
                    object

        -- To be fix
        Object attri ->
            case attri.boundingBox of
                Box box_attri ->
                    object |> scale3D (scale scaleTo (boxSize box_attri))

                None ->
                    object



-- To be fix


{-| Take a list of Objects and group it together into one
-}
group3D : List (Object coordinates) -> Object coordinates
group3D entitybBoxList =
    let
        boxList =
            List.foldl
                (\entity otherEntity ->
                    case entity of
                        ObjectGroup attributes ->
                            case attributes.boundingBox of
                                Box boundingbox ->
                                    boundingbox :: otherEntity

                                _ ->
                                    otherEntity

                        Object attributes ->
                            case attributes.boundingBox of
                                Box boundingbox ->
                                    boundingbox :: otherEntity

                                _ ->
                                    otherEntity
                )
                []
                entitybBoxList

        objListHelper entityList =
            List.foldr
                (\entity otherEntity ->
                    case entity of
                        ObjectGroup attributes ->
                            objListHelper attributes.subObjects ++ otherEntity

                        Object attributes ->
                            attributes :: otherEntity
                )
                []
                entityList

        objList =
            objListHelper entitybBoxList
    in
    case List.head boxList of
        Just boxList_head ->
            case List.tail boxList of
                Just boxList_tail ->
                    ObjectGroup
                        { subObjects = entitybBoxList
                        , boundingBox = Box (BoundingBox3d.aggregate boxList_head boxList_tail)
                        , name = "group"
                        }

                Nothing ->
                    case List.head objList of
                        Just objList_head ->
                            Object objList_head

                        Nothing ->
                            Object
                                { shape = Scene3d.nothing
                                , boundingBox = None
                                , name = "empty"
                                , meshHash = ""
                                , customMesh = Primitive
                                , approxShape = EmptyShape
                                , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
                                }

        Nothing ->
            case List.head objList of
                Just objList_head ->
                    Object objList_head

                Nothing ->
                    Object
                        { shape = Scene3d.nothing
                        , boundingBox = None
                        , name = "empty"
                        , meshHash = ""
                        , customMesh = Primitive
                        , approxShape = EmptyShape
                        , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
                        }



-- MACOUTREACH.ROCKS COMMON FUNCTIONS
-- repeat an animation for a given duration


repeatDuration : Float -> Int -> Float -> Float -> Float
repeatDuration speed duration startPosition time =
    speed * (time - toFloat duration * toFloat (floor time // duration)) + startPosition


repeatDistance : Float -> Float -> Float -> Float -> Float
repeatDistance speed distance startPosition time =
    repeatDuration speed (round <| distance / speed) startPosition time



-- sequence a series of animation pieces together into one


animationPieces : List ( Float, Float -> anytype ) -> (Float -> anytype) -> Float -> anytype
animationPieces intervals finalAnimation time =
    case intervals of
        ( duration, animation ) :: rest ->
            if time <= duration then
                animation time

            else
                animationPieces rest finalAnimation (time - duration)

        [] ->
            finalAnimation time



-- MESHES ARE STORED HERE


dynamicMesh :
    TriangularMesh { position : Point3d Meters coordinates, uv : ( Float, Float ) }
    -> Bool
    -> Mesh.Textured coordinates
dynamicMesh mesh doCulling =
    if doCulling then
        Mesh.texturedFacets mesh |> Mesh.cullBackFaces

    else
        Mesh.texturedFacets mesh


ringMesh : Float -> Float -> Mesh.Textured coordinates
ringMesh radius thickness =
    let
        numPoints =
            1572

        pointList =
            List.map
                (\idx ->
                    let
                        ratioSlow =
                            2 * pi / numPoints

                        ratioFast =
                            200 * pi / numPoints

                        t =
                            toFloat idx

                        slowx =
                            sin (ratioSlow * t)

                        slowy =
                            cos (ratioSlow * t)

                        fastx =
                            radius + thickness * sin (ratioFast * t)

                        fastz =
                            thickness * cos (ratioFast * t)
                    in
                    { position =
                        Point3d.centimeters
                            (slowx * fastx)
                            (slowy * fastx)
                            fastz
                    , uv = ( 0, 0 ) -- Not mapped properly
                    }
                )
            <|
                List.range 0 numPoints

        triangularMesh =
            TriangularMesh.indexed
                (Array.fromList pointList)
                ((List.concatMap
                    (\idx ->
                        [ ( idx + 1, idx, 16 + idx )
                        , ( idx + 1, 16 + idx, 16 + idx + 1 )
                        ]
                    )
                  <|
                    List.range 0 (numPoints - 16)
                 )
                    ++ (List.concatMap
                            (\idx ->
                                [ ( idx + numPoints - 16 + 1, idx + numPoints - 16, 16 + idx )
                                , ( idx + numPoints - 16 + 1, 16 + idx, 16 + idx + 1 )
                                ]
                            )
                        <|
                            List.range -16 0
                       )
                )
    in
    Mesh.texturedFacets triangularMesh |> Mesh.cullBackFaces



-- Modified from "sphere" in elm-3d-scene/src/Scene3d/Primitives.elm so might behave a bit strangely


ellipsoidMesh : Float -> Float -> Float -> Mesh.Textured coordinates
ellipsoidMesh length width height =
    let
        n =
            72

        len =
            Length.centimeters length

        wid =
            Length.centimeters width

        hei =
            Length.centimeters height

        m =
            ceiling (toFloat n / 2)

        thetaValues =
            Parameter1d.steps n
                (Quantity.interpolateFrom Quantity.zero (Angle.turns 1))

        phiValues =
            Parameter1d.steps m
                (Quantity.interpolateFrom
                    (Angle.degrees 90)
                    (Angle.degrees -90)
                )

        vertices =
            thetaValues
                |> List.map
                    (\theta ->
                        phiValues
                            |> List.map
                                (\phi ->
                                    { position =
                                        Point3d.xyz
                                            (len |> Quantity.multiplyBy (Angle.cos phi * Angle.cos theta))
                                            (wid |> Quantity.multiplyBy (Angle.cos phi * Angle.sin theta))
                                            (hei |> Quantity.multiplyBy (Angle.sin phi))
                                    , normal =
                                        Direction3d.xyZ theta phi |> Direction3d.toVector
                                    , uv =
                                        ( Quantity.ratio theta (Angle.turns 1)
                                        , Quantity.ratio
                                            (phi |> Quantity.plus (Angle.degrees 90))
                                            (Angle.degrees 180)
                                        )

                                    -- , tangent =
                                    --     Direction3d.xy (theta |> Quantity.plus (Angle.degrees 90))
                                    --         |> Direction3d.toVector
                                    }
                                )
                    )
                |> List.concat
                |> Array.fromList

        thetaStartIndices =
            List.range 0 (n - 1)

        phiStartIndices =
            List.range 0 (m - 1)

        linearIndex i j =
            i * (m + 1) + j

        faces =
            thetaStartIndices
                |> List.map
                    (\i ->
                        phiStartIndices
                            |> List.map
                                (\j ->
                                    let
                                        bottomLeftIndex =
                                            linearIndex i (j + 1)

                                        bottomRightIndex =
                                            linearIndex (i + 1) (j + 1)

                                        topLeftIndex =
                                            linearIndex i j

                                        topRightIndex =
                                            linearIndex (i + 1) j
                                    in
                                    [ ( bottomLeftIndex
                                      , bottomRightIndex
                                      , topRightIndex
                                      )
                                    , ( bottomLeftIndex
                                      , topRightIndex
                                      , topLeftIndex
                                      )
                                    ]
                                )
                            |> List.concat
                    )
                |> List.concat
    in
    Mesh.texturedFaces (TriangularMesh.indexed vertices faces)
        |> Mesh.cullBackFaces


polyCylinderMesh : List ( Float, Float ) -> Float -> Mesh.Textured coordinates
polyCylinderMesh points height =
    let
        bottom =
            List.map
                (\( x, y ) ->
                    { position = Point3d.centimeters x y 0
                    , uv = ( (x - minX) / uWidth, (y - minY) / vWidth )
                    }
                )
                points

        top =
            List.map
                (\( x, y ) ->
                    { position = Point3d.centimeters x y height
                    , uv = ( (x - minX) / uWidth, (y - minY) / vWidth )
                    }
                )
                points

        minX =
            Maybe.withDefault 0 (List.minimum (List.map (\( x, _ ) -> x) points))

        maxX =
            Maybe.withDefault 0 (List.maximum (List.map (\( x, _ ) -> x) points))

        minY =
            Maybe.withDefault 0 (List.minimum (List.map (\( _, y ) -> y) points))

        maxY =
            Maybe.withDefault 0 (List.maximum (List.map (\( _, y ) -> y) points))

        uWidth =
            maxX - minX

        vWidth =
            maxY - minY

        bottomOrigin =
            { position = Point3d.centimeters ((maxX + minX) / 2) ((maxY + minY) / 2) 0, uv = ( 0.5, 0.5 ) }

        bottomFace =
            TriangularMesh.fan bottomOrigin (List.reverse bottom)

        topOrigin =
            { position = Point3d.centimeters ((maxX + minX) / 2) ((maxY + minY) / 2) height, uv = ( 0.5, 0.5 ) }

        topFace =
            TriangularMesh.fan topOrigin top

        sides =
            TriangularMesh.strip bottom top
    in
    TriangularMesh.combine [ sides, bottomFace, topFace ]
        |> Mesh.texturedFacets
        |> Mesh.cullBackFaces


polyConeMesh : List ( Float, Float ) -> ( Float, Float, Float ) -> Mesh.Textured coordinates
polyConeMesh points ( xtip, ytip, ztip ) =
    let
        tip =
            { position = Point3d.centimeters xtip ytip ztip
            , uv = ( (xtip - minX) / uWidth, (ytip - minY) / vWidth )
            }

        apron =
            List.map
                (\( x, y ) ->
                    { position = Point3d.centimeters x y 0
                    , uv = ( (x - minX) / uWidth, (y - minY) / vWidth )
                    }
                )
                points

        pointsIncludingTip =
            ( xtip, ytip ) :: points

        -- Used for UV mapping, so only 2D coordinates are required
        minX =
            Maybe.withDefault 0 (List.minimum (List.map (\( x, _ ) -> x) pointsIncludingTip))

        maxX =
            Maybe.withDefault 0 (List.maximum (List.map (\( x, _ ) -> x) pointsIncludingTip))

        minY =
            Maybe.withDefault 0 (List.minimum (List.map (\( _, y ) -> y) pointsIncludingTip))

        maxY =
            Maybe.withDefault 0 (List.maximum (List.map (\( _, y ) -> y) pointsIncludingTip))

        uWidth =
            maxX - minX

        vWidth =
            maxY - minY

        bottomOrigin =
            { position = Point3d.centimeters ((maxX + minX) / 2) ((maxY + minY) / 2) 0, uv = ( 0.5, 0.5 ) }

        bottomFace =
            TriangularMesh.fan bottomOrigin (List.reverse apron)
    in
    TriangularMesh.combine [ bottomFace, TriangularMesh.fan tip apron ]
        |> Mesh.texturedFacets
        |> Mesh.cullBackFaces


{-| Creates a 3D mesh representing a 2D polygon given the number of sides.
Number of sides is automatically adjusted to be at least 3.
Since this mesh is quite simple, dynamically generating it should be fine.
-}
polygonMesh : Int -> Mesh.Textured coordinates
polygonMesh sides =
    let
        -- Can't really have a polygon with less than 3 sides
        actualSides =
            if sides < 3 then
                3

            else
                sides

        angleDegrees =
            360 / toFloat actualSides

        getPos n =
            Point3d.translateBy (Vector3d.withLength (Length.centimeters 1) (Direction3d.xy (Angle.degrees (angleDegrees * n)))) Point3d.origin

        getRange values =
            ( Maybe.withDefault 0 (List.maximum values), Maybe.withDefault 0 (List.minimum values) )

        xCoords =
            List.map (\point -> Length.inMeters (Point3d.xCoordinate point)) (List.map (\n -> getPos (toFloat n)) (List.range 0 (actualSides - 1)))

        yCoords =
            List.map (\point -> Length.inMeters (Point3d.yCoordinate point)) (List.map (\n -> getPos (toFloat n)) (List.range 0 (actualSides - 1)))

        uWidth =
            Tuple.first (getRange xCoords) - Tuple.second (getRange xCoords)

        vWidth =
            Tuple.first (getRange yCoords) - Tuple.second (getRange yCoords)

        points =
            List.map
                (\n ->
                    { position = getPos n
                    , uv = ( (Length.inMeters (Point3d.xCoordinate (getPos n)) - Tuple.second (getRange xCoords)) / uWidth, (Length.inMeters (Point3d.yCoordinate (getPos n)) - Tuple.second (getRange yCoords)) / vWidth )
                    }
                )
                (List.map toFloat (List.range 0 actualSides))

        polyOrigin =
            let
                default =
                    { position = Point3d.origin, uv = ( 0.5, 0.5 ) }
            in
            Maybe.withDefault default (List.head points)
    in
    TriangularMesh.fan polyOrigin points
        |> Mesh.texturedFacets


customPolyMesh : List ( Float, Float ) -> Float -> Float -> Float -> Float -> Mesh.Textured coordinates
customPolyMesh points maxX minX maxY minY =
    let
        uWidth =
            maxX - minX

        vWidth =
            maxY - minY

        tip =
            { position = Point3d.centimeters ((maxX + minX) / 2) ((maxY + minY) / 2) 0
            , uv = ( 0.5, 0.5 ) -- Since the position takes the average of X and Y coordinates, this should be in the middle
            }

        apron =
            List.map
                (\( x, y ) ->
                    { position = Point3d.centimeters x y 0
                    , uv = ( (x - minX) / uWidth, (y - minY) / vWidth )
                    }
                )
                points
    in
    TriangularMesh.fan tip apron
        |> Mesh.texturedFacets


texturedCylinderMesh : Float -> Float -> Mesh.Textured coordinates
texturedCylinderMesh r h =
    let
        radius =
            Length.centimeters r

        subdivisions =
            72

        height =
            Length.centimeters h

        wedgeAngle =
            Angle.turns 1 |> Quantity.divideBy (toFloat subdivisions)

        negativeZVector =
            Direction3d.negativeZ |> Direction3d.toVector

        positiveZVector =
            Direction3d.positiveZ |> Direction3d.toVector

        bottomZ =
            Quantity.zero

        topZ =
            height

        bottomCenter =
            Point3d.xyz Quantity.zero Quantity.zero bottomZ

        topCenter =
            Point3d.xyz Quantity.zero Quantity.zero topZ

        wedge startIndex =
            let
                startAngle =
                    wedgeAngle |> Quantity.multiplyBy (toFloat startIndex)

                endIndex =
                    startIndex + 1 |> modBy subdivisions

                endAngle =
                    wedgeAngle |> Quantity.multiplyBy (toFloat endIndex)

                startX =
                    radius |> Quantity.multiplyBy (Angle.cos startAngle)

                endX =
                    radius |> Quantity.multiplyBy (Angle.cos endAngle)

                startY =
                    radius |> Quantity.multiplyBy (Angle.sin startAngle)

                endY =
                    radius |> Quantity.multiplyBy (Angle.sin endAngle)

                p0 =
                    Point3d.xyz startX startY bottomZ

                p1 =
                    Point3d.xyz endX endY bottomZ

                p2 =
                    Point3d.xyz startX startY topZ

                p3 =
                    Point3d.xyz endX endY topZ

                startNormal =
                    Direction3d.on SketchPlane3d.xy
                        (Direction2d.fromAngle startAngle)
                        |> Direction3d.toVector

                endNormal =
                    Direction3d.on SketchPlane3d.xy
                        (Direction2d.fromAngle endAngle)
                        |> Direction3d.toVector

                uStart =
                    Angle.inRadians startAngle / (Angle.inRadians wedgeAngle * subdivisions)

                uEnd =
                    -- I'm doing this check since at the very end, endAngle is 0 because of how it's calculated
                    if (Angle.inRadians endAngle |> abs) < 1.0e-6 then
                        1

                    else
                        Angle.inRadians endAngle / (Angle.inRadians wedgeAngle * subdivisions)
            in
            [ ( { position = bottomCenter, normal = negativeZVector, uv = ( 0.5, 0 ) }
              , { position = p1, normal = negativeZVector, uv = ( uEnd, 0 ) }
              , { position = p0, normal = negativeZVector, uv = ( uStart, 0 ) }
              )
            , ( { position = p0, normal = startNormal, uv = ( uStart, 0 ) }
              , { position = p1, normal = endNormal, uv = ( uEnd, 0 ) }
              , { position = p3, normal = endNormal, uv = ( uEnd, 1 ) }
              )
            , ( { position = p0, normal = startNormal, uv = ( uStart, 0 ) }
              , { position = p3, normal = endNormal, uv = ( uEnd, 1 ) }
              , { position = p2, normal = startNormal, uv = ( uStart, 1 ) }
              )
            , ( { position = topCenter, normal = positiveZVector, uv = ( 0.5, 1 ) }
              , { position = p2, normal = positiveZVector, uv = ( uStart, 1 ) }
              , { position = p3, normal = positiveZVector, uv = ( uEnd, 1 ) }
              )
            ]

        wedges =
            List.range 0 (subdivisions - 1)
                |> List.map wedge

        triangularMesh =
            TriangularMesh.triangles (List.concat wedges)
    in
    Mesh.texturedFaces triangularMesh |> Mesh.cullBackFaces


truncatedConeMesh : Float -> Float -> Float -> Mesh.Textured coordinates
truncatedConeMesh topR botR height =
    let
        subdivisions =
            36

        angleDegrees =
            360 / subdivisions

        getU n =
            if n == 0 then
                1.0e-3

            else if n == 36 then
                1 - 1.0e-3

            else
                (n * angleDegrees) / (36 * angleDegrees)

        topPoints =
            List.map
                (\n ->
                    { position =
                        Point3d.centimeters 0 0 height
                            |> Point3d.translateBy (Vector3d.withLength (Length.centimeters topR) (Direction3d.xy (Angle.degrees (angleDegrees * n))))
                    , uv = ( getU n / 2 + 0.5, 1 )
                    }
                )
                (List.map toFloat (List.range 0 36))

        topFacePoints =
            List.map
                (\n ->
                    { position =
                        Point3d.centimeters 0 0 height
                            |> Point3d.translateBy (Vector3d.withLength (Length.centimeters topR) (Direction3d.xy (Angle.degrees (angleDegrees * n))))
                    , uv = ( 0.25 * cos (degrees (angleDegrees * n)) + 0.25, 0.5 * sin (degrees (angleDegrees * n)) + 0.5 )
                    }
                )
                (List.map toFloat (List.range 0 36))

        topOrigin =
            { position = Point3d.centimeters 0 0 height
            , uv = ( 0.25, 0.5 )
            }

        bottomPoints =
            List.map
                (\n ->
                    { position =
                        Point3d.origin
                            |> Point3d.translateBy (Vector3d.withLength (Length.centimeters botR) (Direction3d.xy (Angle.degrees (angleDegrees * n))))
                    , uv = ( getU n / 2 + 0.5, 0 )
                    }
                )
                (List.map toFloat (List.range 0 36))

        face =
            TriangularMesh.fan topOrigin topFacePoints

        sides =
            TriangularMesh.strip bottomPoints topPoints
    in
    TriangularMesh.combine [ face, sides ]
        |> Mesh.texturedFacets


type alias GeneratedMesh coordinates =
    { name : String
    , mesh : Mesh.Textured coordinates
    , shadow : Mesh.Shadow coordinates
    }


{-| Get the last `n` digits of a float as a string.
-}
lastDigits : Int -> Float -> String
lastDigits n val =
    let
        stringVal =
            String.fromFloat val
                |> String.replace "." ""
    in
    String.right n stringVal


{-| Get an identifier for a `CustomMesh` based on its properties
-}
getMeshName : CustomMesh coordinates -> String
getMeshName mesh =
    let
        hashToUnicode ks =
            case ks of
                [] ->
                    ""

                k :: rest ->
                    let
                        -- Maximum for Unicode characters
                        actualK =
                            if k < 0 then
                                k * -pi

                            else
                                k * sqrt 2

                        a =
                            0.5 * (sqrt 5 - 1)

                        s =
                            actualK * a

                        x =
                            s - toFloat (floor s)

                        hk =
                            floor (2 ^ 16 * x)

                        segment =
                            getUnicodeSegment hk (2 ^ 12) ""
                    in
                    segment ++ hashToUnicode rest

        getUnicodeSegment k step curSegment =
            let
                actualStep =
                    if step >= max - min then
                        (max - min) // 2

                    else
                        step

                max =
                    0x0010FFFF

                min =
                    0
            in
            if k > max then
                getUnicodeSegment
                    (k - actualStep)
                    actualStep
                    (curSegment ++ String.fromChar (Char.fromCode (k |> modBy max)))

            else if k < min then
                getUnicodeSegment
                    (k + actualStep)
                    actualStep
                    (curSegment ++ String.fromChar (Char.fromCode (max - k |> modBy max)))

            else
                curSegment ++ String.fromChar (Char.fromCode k)

        hashNum k =
            let
                a =
                    0.5 * (sqrt 5 - 1)

                s =
                    k * a

                x =
                    s - toFloat (floor s)
            in
            String.fromInt (floor (2 ^ 16 * x))
    in
    case mesh of
        Primitive ->
            ""

        Ellipsoid ( length, width, height ) ->
            "ellipsoid-" ++ String.fromFloat length ++ String.fromFloat width ++ String.fromFloat height

        Ring radius thickness ->
            "ring" ++ String.fromFloat radius ++ String.fromFloat thickness

        PolyCone points ( xtip, ytip, ztip ) ->
            "polyCone-" ++ hashToUnicode (List.map Tuple.first points) ++ "-" ++ hashToUnicode (List.map Tuple.second points) ++ "-" ++ hashToUnicode [ xtip, ytip, ztip ]

        PolyCylinder points height ->
            "polyCylinder-" ++ hashToUnicode (List.map Tuple.first points) ++ "-" ++ hashToUnicode (List.map Tuple.second points) ++ "-" ++ hashNum height

        TexturedCylinder radius height ->
            "texturedCylinder" ++ String.fromFloat radius ++ String.fromFloat height

        TruncatedCone topR botR height ->
            "truncatedCone" ++ String.fromFloat topR ++ String.fromFloat botR ++ String.fromFloat height

        CustomObject triangularMesh doCulling ->
            let
                posID =
                    TriangularMesh.vertices triangularMesh
                        |> Array.toList
                        |> List.map (\vertex -> vertex.position)
                        |> List.map (Point3d.toTuple Length.inCentimeters)
                        |> List.map (\( x, y, z ) -> x + y - z)
                        |> hashToUnicode

                uvID =
                    TriangularMesh.vertices triangularMesh
                        |> Array.toList
                        |> List.map (\vertex -> vertex.uv)
                        |> List.map (\( u, v ) -> (u + v) * (pi / 4))
                        |> hashToUnicode

                cullMarker =
                    if doCulling then
                        "-culled"

                    else
                        ""
            in
            "customObject-" ++ posID ++ "-" ++ uvID ++ cullMarker


generateEllipsoid : Float -> Float -> Float -> GeneratedMesh coordinates
generateEllipsoid length width height =
    { name = getMeshName (Ellipsoid ( length, width, height ))
    , mesh = ellipsoidMesh length width height
    , shadow = Mesh.shadow (ellipsoidMesh length width height)
    }


generatePolyCylinder : List ( Float, Float ) -> Float -> GeneratedMesh coordinates
generatePolyCylinder points height =
    { name = getMeshName (PolyCylinder points height)
    , mesh = polyCylinderMesh points height
    , shadow = Mesh.shadow (polyCylinderMesh points height)
    }


generatePolyCone : List ( Float, Float ) -> ( Float, Float, Float ) -> GeneratedMesh coordinates
generatePolyCone points ( xtip, ytip, ztip ) =
    { name = getMeshName (PolyCone points ( xtip, ytip, ztip ))
    , mesh = polyConeMesh points ( xtip, ytip, ztip )
    , shadow = Mesh.shadow (polyConeMesh points ( xtip, ytip, ztip ))
    }


generateRing : Float -> Float -> GeneratedMesh coordinates
generateRing radius thickness =
    { name = getMeshName (Ring radius thickness)
    , mesh = ringMesh radius thickness
    , shadow = Mesh.shadow (ringMesh radius thickness)
    }


generateTexturedCylinder : Float -> Float -> GeneratedMesh coordinates
generateTexturedCylinder radius height =
    { name = getMeshName (TexturedCylinder radius height)
    , mesh = texturedCylinderMesh radius height
    , shadow = Mesh.shadow (texturedCylinderMesh radius height)
    }


generateTruncatedCone : Float -> Float -> Float -> GeneratedMesh coordinates
generateTruncatedCone topR botR height =
    { name = getMeshName (TruncatedCone topR botR height)
    , mesh = truncatedConeMesh topR botR height
    , shadow = Mesh.shadow (truncatedConeMesh topR botR height)
    }


generateDynamicMesh :
    TriangularMesh { position : Point3d Meters coordinates, uv : ( Float, Float ) }
    -> Bool
    -> GeneratedMesh coordinates
generateDynamicMesh triangularMesh doCulling =
    { name = getMeshName (CustomObject triangularMesh doCulling)
    , mesh = dynamicMesh triangularMesh doCulling
    , shadow = Mesh.shadow (dynamicMesh triangularMesh doCulling)
    }


withOverlay :
    List (G.Shape msg)
    -> { a | widget : Widget.Model, height : Quantity Int Pixels, width : Quantity Int Pixels, time : Float }
    -> Html msg
    -> Html msg
withOverlay shapes model html =
    Html.div []
        [ Html.div [ HA.style "position" "absolute", HA.style "top" "0px", HA.style "left" "0px" ]
            [ html
            ]
        , Html.div [ HA.style "position" "absolute", HA.style "top" "0px", HA.style "left" "0px", HA.style "width" (String.fromInt (unwrapQ model.width) ++ "px"), HA.style "height" (String.fromInt (unwrapQ model.height) ++ "px") ]
            [ Widget.view model.widget shapes
            ]
        ]


unwrapQ : Quantity number b -> number
unwrapQ (Quantity.Quantity q) =
    q


{-| Takes a list of objects and extracts the entities from them
-}
renderEntities : List (Object coordinates) -> List (Entity coordinates)
renderEntities objects =
    let
        extract =
            \obj ->
                case obj of
                    ObjectGroup attributes ->
                        Scene3d.group (renderEntities attributes.subObjects)

                    Object attributes ->
                        attributes.shape
    in
    List.map extract objects


{-| Signifies that the collider of an object should be rendered as well as the object itself.
Colliders will start out as red, and change colour as group depth increases.
-}
renderCollider : Int -> Object coordinates -> Object coordinates
renderCollider maxDepth object =
    renderColliderHelper maxDepth 0 object


{-| Helper function for `renderCollider`
-}
renderColliderHelper : Int -> Int -> Object coordinates -> Object coordinates
renderColliderHelper maxDepth n object =
    let
        hue =
            clamp 0.0 1.0 (toFloat n * 0.1)

        colour =
            Color.hsl hue 1 0.5

        collider boundingBox =
            boxOutline (Material.color colour) boundingBox

        colliderObj bBox =
            Object
                { shape = collider bBox
                , boundingBox = None
                , name = "renderedCollider"
                , meshHash = ""
                , customMesh = Primitive
                , approxShape = EmptyShape
                , rotation = { pitch = Quantity.zero, roll = Quantity.zero, yaw = Quantity.zero }
                }
    in
    if n >= maxDepth then
        object

    else
        case object of
            ObjectGroup attributes ->
                case attributes.boundingBox of
                    Box boundingBox ->
                        ObjectGroup
                            { attributes
                                | subObjects = colliderObj boundingBox :: List.map (renderColliderHelper maxDepth (n + 1)) attributes.subObjects
                            }

                    None ->
                        object

            Object attributes ->
                case attributes.boundingBox of
                    Box boundingBox ->
                        Object { attributes | shape = Scene3d.group [ attributes.shape, collider boundingBox ] }

                    None ->
                        object


relativeP : Float -> Quantity Int Pixels -> Float
relativeP blockAmount windowSize =
    toFloat (unwrapQ windowSize) / blockAmount


getVolume : Object coordinates -> Volume
getVolume object =
    case object of
        ObjectGroup attr ->
            List.foldl Quantity.plus Quantity.zero (List.map getVolume attr.subObjects)

        Object attr ->
            case attr.approxShape of
                EmptyShape ->
                    Volume.cubicMeters 0

                Block shape ->
                    Block3d.volume shape

                Sphere shape ->
                    Sphere3d.volume shape

                EllipsoidShape _ ( l, w, h ) ->
                    ((4 / 3) * pi * l * w * h)
                        |> Volume.milliliters

                RingShape outer inner ->
                    Cylinder3d.volume outer
                        |> Quantity.minus (Cylinder3d.volume inner)

                Cone shape ->
                    Cone3d.volume shape

                Cylinder shape ->
                    Cylinder3d.volume shape


getCollider : Object coordinates -> Block3d Meters coordinates
getCollider obj =
    case obj of
        ObjectGroup attr ->
            case attr.boundingBox of
                Box bBox ->
                    Block3d.fromBoundingBox bBox

                None ->
                    Block3d.from (getCentre obj) (getCentre obj)

        Object attr ->
            case attr.boundingBox of
                Box bBox ->
                    Block3d.fromBoundingBox bBox

                None ->
                    Block3d.from (getCentre obj) (getCentre obj)


getPosition : Object coordinates -> Point3d Meters a
getPosition obj =
    Point3d.placeIn Frame3d.atOrigin (Block3d.centerPoint (getCollider obj))


getCentre : Object coordinates -> Point3d Meters coordinates
getCentre obj =
    case obj of
        ObjectGroup attr ->
            case attr.boundingBox of
                Box bBox ->
                    BoundingBox3d.centerPoint bBox

                _ ->
                    case List.head attr.subObjects of
                        Nothing ->
                            Point3d.origin

                        Just subObj ->
                            getCentre subObj

        Object attr ->
            case attr.approxShape of
                EmptyShape ->
                    Point3d.origin

                Block shape ->
                    Block3d.centerPoint shape

                Sphere shape ->
                    Sphere3d.centerPoint shape

                EllipsoidShape _ ( l, w, h ) ->
                    Block3d.centerPoint (Block3d.from (Point3d.centimeters (l / 2) (w / 2) (h / 2)) (Point3d.centimeters (l / 2) (w / 2) (h / 2)))

                RingShape outer _ ->
                    Cylinder3d.centerPoint outer

                Cone shape ->
                    Point3d.midpoint (Cone3d.tipPoint shape) (Cone3d.basePoint shape)

                Cylinder shape ->
                    Cylinder3d.centerPoint shape
