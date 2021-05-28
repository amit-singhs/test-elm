module PortsPos exposing (..)

--import Element.Text as text exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (username)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Main exposing (Msg(..))



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Loading



-- UPDATE


type Msg
    = SayHello
    | DoNothing String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SayHello ->
            ( Loading, Cmd.none )

        DoNothing inputString ->
            ( Loading, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        createButton buttonLabel buttonlength buttonEvent tL tR bL bR r g b =
            Input.button
                [ Element.height (px 60)
                , Element.width (px buttonlength)
                , Border.width 1
                , Border.roundEach
                    { topLeft = tL
                    , topRight = tR
                    , bottomLeft = bL
                    , bottomRight = bR
                    }
                , Border.color <| Element.rgb255 84 83 81
                , Font.size 25
                , Font.family
                    [ Font.typeface "Helvetica"
                    ]
                , Background.color <| Element.rgb255 r g b
                , Font.color <| Element.rgb255 228 228 228
                , Font.medium
                , Font.center
                , mouseDown
                    [ Background.color <| Element.rgb255 180 180 179
                    , Border.color <| Element.rgb255 84 83 81
                    ]
                ]
                { onPress = Just buttonEvent
                , label = Element.text buttonLabel
                }
    in
    Element.layout [] <|
        row [ padding 40 ]
            [ column [ Element.width fill ]
                [ row []
                    [ Input.text
                        [ Border.roundEach
                            { topLeft = 12
                            , topRight = 12
                            , bottomLeft = 0
                            , bottomRight = 0
                            }
                        , Element.width (px 495)
                        , Border.color <| Element.rgb255 84 83 81
                        , padding 40
                        , Background.color <| Element.rgb255 174 245 189
                        , Font.light
                        , Font.alignRight
                        , Font.color <| Element.rgb255 0 0 0
                        ]
                        { label = Input.labelHidden "text box"
                        , onChange = DoNothing
                        , placeholder = Nothing
                        , text = "0"
                        }
                    ]
                , row []
                    [ column [] [ createButton "1" 165 (DoNothing "") 0 0 0 0 106 166 119 ]
                    , column [] [ createButton "2" 165 (DoNothing "") 0 0 0 0 106 166 119 ]
                    , column [] [ createButton "3" 165 (DoNothing "") 0 0 0 0 106 166 119 ]
                    ]
                , row []
                    [ column [] [ createButton "4" 165 (DoNothing "") 0 0 0 0 106 166 119 ]
                    , column [] [ createButton "5" 165 (DoNothing "") 0 0 0 0 106 166 119 ]
                    , column [] [ createButton "6" 165 (DoNothing "") 0 0 0 0 106 166 119 ]
                    ]
                , row []
                    [ column [] [ createButton "7" 165 (DoNothing "") 0 0 0 0 106 166 119 ]
                    , column [] [ createButton "8" 165 (DoNothing "") 0 0 0 0 106 166 119 ]
                    , column [] [ createButton "9" 165 (DoNothing "") 0 0 0 0 106 166 119 ]
                    ]
                , row []
                    [ column [] [ createButton "." 165 (DoNothing "") 0 0 0 0 106 166 119 ]
                    , column [] [ createButton "0" 165 (DoNothing "") 0 0 0 0 106 166 119 ]
                    , column [] [ createButton "Add" 165 (DoNothing "") 0 0 0 0 106 166 119 ]
                    ]
                , row []
                    [ column [] [ createButton "Pay" 495 (DoNothing "") 0 0 0 0 106 166 119 ]
                    ]
                ]
            , column [ padding 10 ] []
            , column []
                [ row []
                    [ Input.text
                        [ Border.roundEach
                            { topLeft = 12
                            , topRight = 12
                            , bottomLeft = 12
                            , bottomRight = 12
                            }
                        , Element.width (px 250)
                        , Element.height (px 650)
                        , Border.color <| Element.rgb255 84 83 81
                        , padding 40
                        , Background.color <| Element.rgb255 174 245 189
                        , Font.light
                        , Font.alignRight
                        , Font.color <| Element.rgb255 0 0 0
                        ]
                        { label = Input.labelHidden "text box"
                        , onChange = DoNothing
                        , placeholder = Nothing
                        , text = "0"
                        }
                    ]
                ]
            ]
