module TypingApp exposing (..)

{-
   <b>Animation Slot</b>: Make an animation here!
-}

import Array
import Browser
import Browser.Dom exposing (Viewport, getViewportOf)
import Browser.Events exposing (onAnimationFrame, onKeyDown, onKeyPress, onKeyUp, onResize)
import Browser.Navigation exposing (Key)
import Char
import GraphicSVG exposing (..)
import Html
import Json.Decode as D
import List exposing (range)
import String
import Svg
import Svg.Attributes as SA
import Task
import Time exposing (..)



-- import Extra exposing(..)
{- StartingCode -}
-- use these variables for your collage size


collageWidth =
    200


collageHeight =
    128


appTitle =
    "My App"



-- depending on the state you can turn on and off typing


allowTyping model =
    model.state /= NotTyping



-- depending on state you can turn on and off animation (the Tick message)


isAnimating model =
    False


myShapes model =
    [ textBox 200 128 True [ "Anurag" ]
    ]


textBox width height isHighlighted chars =
    [ rect width height |> filled white
    , text (String.join "" <| List.reverse chars)
        |> centered
        |> size 4
        |> filled black
        |> clip (rect width height |> ghost)
    , rect width height
        |> outlined (solid 1)
            (if isHighlighted then
                rgb 0 0 255

             else
                charcoal
            )
    ]
        |> group


type Msg
    = Tick Float
    | WindowResize (Maybe ( Float, Float ))
    | ReturnPosition (( Float, Float ) -> Msg) ( Float, Float )
    | KeyDown String
    | KeyUp String
    | NoOp



-- do not change the messages above, but add your own messages below
-- TODO


type State
    = NotTyping



-- | TODO


type alias Model =
    { state : State -- TODO
    , window : Window -- do not change (used for resizing window)
    , time : Float -- not recommended that you change this
    }


init =
    { state = NotTyping -- TODO

    -- do not change these
    , time = 0
    , window = { cw = collageWidth, ch = collageHeight, sw = 0, sh = 0 }
    }


update msg model =
    case msg of
        -- TODO handle new messages here
        -- get keyboard input
        KeyUp _ ->
            ( model, Cmd.none )

        KeyDown code ->
            ( case model.state of
                NotTyping ->
                    model
              -- TODO handle keyboard input here
              -- ExampleState -> { model | chars1 = typeAndDelete model.chars1 code }
            , Cmd.none
            )

        -- don't change these unless you really need to
        Tick t ->
            ( { model | time = t }, Cmd.none )

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

        ReturnPosition message ( x, y ) ->
            let
                ( newModel, userCmds ) =
                    update
                        (message (convertCoords model.window ( x, y )))
                        model
            in
            ( newModel, userCmds )

        NoOp ->
            ( model, Cmd.none )


typeAndDelete soFar code =
    if String.length code == 1 then
        code :: soFar

    else if code == "Backspace" then
        List.drop 1 soFar

    else
        soFar



{- EndStartingCode -}
{- ViewonlyCode -}


main : Program () Model Msg
main =
    Browser.document
        { init =
            \_ ->
                -- we do not support flags
                ( init, getViewportSize )
        , update = update
        , view = \model -> { body = [ createCollage collageWidth collageHeight <| myShapes model ], title = appTitle }
        , subscriptions = subscriptions
        }


subscriptions model =
    Sub.batch
        (let
            at =
                allowTyping model

            an =
                isAnimating model
         in
         if at && an then
            [ onKeyUp (D.map KeyUp (D.field "key" D.string))
            , onKeyDown (D.map KeyDown (D.field "key" D.string))
            , onResize (\_ _ -> WindowResize Nothing)
            , onAnimationFrame (\posix -> toFloat (Time.posixToMillis posix) * 0.001 |> Tick)
            ]

         else if at then
            [ onKeyUp (D.map KeyUp (D.field "key" D.string))
            , onKeyDown (D.map KeyDown (D.field "key" D.string))
            , onResize (\_ _ -> WindowResize Nothing)
            ]

         else if an then
            [ onResize (\_ _ -> WindowResize Nothing)
            , onAnimationFrame (\posix -> toFloat (Time.posixToMillis posix) * 0.001 |> Tick)
            ]

         else
            [ onResize (\_ _ -> WindowResize Nothing) ]
        )


getViewportSize : Cmd Msg
getViewportSize =
    Task.attempt
        (\rvp ->
            case rvp of
                Ok vp ->
                    WindowResize <|
                        Just ( vp.viewport.width, vp.viewport.height )

                Err _ ->
                    NoOp
        )
        (getViewportOf "render")


type alias Window =
    { cw : Float
    , ch : Float
    , sw : Float
    , sh : Float
    }


didResize window sw sh =
    { window | sw = sw, sh = sh }


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
                sw / sh

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
                sw / cw

            else if scaledInY then
                sh / ch

            else
                1
    in
    ( (x - sw / 2) / cscale
    , (y + sh / 2) / cscale
    )


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



-- repeat an animation for a given duration


repeatDuration : Float -> Int -> Float -> Float -> Float
repeatDuration speed duration startPosition time =
    speed * (time - toFloat duration * toFloat (floor time // duration)) + startPosition


repeatDistance : Float -> Float -> Float -> Float -> Float
repeatDistance speed distance startPosition time =
    repeatDuration speed (round <| distance / speed) startPosition time


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


plotGraph : (Float -> Float) -> Float -> Shape a
plotGraph f time =
    group
        [ openPolygon (List.map (\t -> ( -96 + toFloat t / 2.5 - 200 * toFloat (floor (time / 10)), f (toFloat t / 50) )) <| List.range (500 * floor (time / 10)) (500 * ceiling (time / 10))) |> outlined (solid 1) (rgb 0 0 200)
        , group
            [ circle 3 |> filled red
            , text ("(" ++ String.fromFloat time ++ ", " ++ String.fromFloat (toFloat (round <| f time * 100) / 100) ++ ")")
                |> size 6
                |> filled black
                |> move ( 5, 5 )
            ]
            |> move ( -96 + 20 * time - 200 * toFloat (floor (time / 10)), f time )
        ]



{- EndViewonlyCode -}
