module CallExternalAPI exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)



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
    = Failure
    | Loading
    | Success String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getCryptoSymbolsList )


type alias CryptoInstrument =
    { symbol : String
    , name : String
    , currentPrice : Int
    }



-- UPDATE


type Msg
    = MorePlease
    | GotCryptoList (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, getCryptoSymbolsList )

        GotCryptoList result ->
            case result of
                Ok textValue ->
                    ( Success textValue, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Bitcoin Introduction " ]
        , renderModels model
        ]


renderModels : Model -> Html Msg
renderModels model =
    case model of
        Failure ->
            div []
                [ text "Error encountered : "
                , button [ onClick MorePlease ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success textValue ->
            div []
                [ div [] [ text textValue ]
                ]



-- HTTP


getCryptoSymbolsList : Cmd Msg
getCryptoSymbolsList =
    Http.get
        { url = "https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd"
        , expect = Http.expectJson GotCryptoList cryptoNameDecoder
        }


cryptoNameDecoder : Decoder String
cryptoNameDecoder =
    field "id" string
