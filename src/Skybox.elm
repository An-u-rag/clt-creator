module Skybox exposing (skybox, roundSkybox, SkyboxType(..))

{-| This example shows how to load mulitiple textures 
    and apply it to a simple 3D entity (Box).
-}

import Angle exposing (Angle)
import Array exposing (Array)
import Browser
import Camera3d
import Color exposing (Color)
import Html exposing (Html)
import Length
import Pixels
import Point3d
import Scene3d
import Scene3d.Material as Material
import Task
import Viewpoint3d
import WebGL.Texture
import Sphere3d
import Parameter1d
import Scene3d.Mesh as Mesh
import Quantity
import Direction3d
import TriangularMesh

import GraphicSVG exposing (..)


sphereMeshNoCulling : Float -> Mesh.Textured coordinates
sphereMeshNoCulling radius = 
    let
        n =
            72

        r = Length.centimeters radius

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
                                            (r |> Quantity.multiplyBy (Angle.cos phi * Angle.cos theta))
                                            (r |> Quantity.multiplyBy (Angle.cos phi * Angle.sin theta))
                                            (r |> Quantity.multiplyBy (Angle.sin phi))
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
                |> List.reverse
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

roundSkybox texture size =
    let
        material = Material.texturedColor (texture |> Maybe.withDefault (Material.constant Color.lightBlue))
    in
    Scene3d.mesh material (sphereMeshNoCulling size)

skybox textureList size =
    let
        -- Define the negative and positive X/Y/Z coordinates of a 16 'pixel'
        -- wide cube centered at the origin (see https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Length#cssPixels)
        negative =
            Length.centimeters -size

        positive =
            Length.centimeters size

        -- Define the eight vertices of the cube
        p1 =
            Point3d.xyz negative negative negative

        p2 =
            Point3d.xyz positive negative negative

        p3 =
            Point3d.xyz positive positive negative

        p4 =
            Point3d.xyz negative positive negative

        p5 =
            Point3d.xyz negative negative positive

        p6 =
            Point3d.xyz positive negative positive

        p7 =
            Point3d.xyz positive positive positive

        p8 =
            Point3d.xyz negative positive positive

        -- Create the six faces with different colors
        skybottom texture =
            Scene3d.quad texture p4 p3 p2 p1

        skytop texture =
            Scene3d.quad texture p8 p7 p6 p5
        -- skyside1 to skyside4, see sky-box-order.jpg as the
        -- order reference
        -- corresponding to the picture from left to right
        skyside1 texture =
            Scene3d.quad texture p1 p4 p8 p5

        skyside2 texture =
            Scene3d.quad texture p4 p3 p7 p8

        skyside3 texture =
            Scene3d.quad texture p3 p2 p6 p7

        skyside4 texture =
            Scene3d.quad texture p2 p1 p5 p6
    in
      -- Combine all faces into a single entity
      -- Decode the list here
      case textureList of
         (Just bottom) :: (Just top) :: (Just side1) :: (Just side2) 
           :: (Just side3) :: (Just side4) :: _ ->
            let
              debottom = Material.texturedColor bottom
              detop = Material.texturedColor top
              deside1 = Material.texturedColor side1
              deside2 = Material.texturedColor side2
              deside3 = Material.texturedColor side3
              deside4 = Material.texturedColor side4
            in
              Scene3d.group [ skybottom debottom, skytop detop
                , skyside1 deside1, skyside2 deside2
                , skyside3 deside3, skyside4 deside4 ]
         _ ->
            let
              fallbackMat = Material.texturedColor (Material.constant Color.lightBlue)
            in
              Scene3d.group [ skybottom fallbackMat, skytop fallbackMat
                , skyside1 fallbackMat, skyside2 fallbackMat
                , skyside3 fallbackMat, skyside4 fallbackMat ]

type SkyboxType msg
    = URLSkybox String String String String String String Float
    | GSVGSkybox Bool (Shape msg) (Shape msg) (Shape msg) Float
    | URLSphericalSkybox String Float
    | GSVGSphericalSkybox Bool (Shape msg) Float
    | BasicSkybox Color.Color
