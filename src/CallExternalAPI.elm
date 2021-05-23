module CallExternalAPI exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, float, int, list, map3, string)
import Json.Encode exposing (float)



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
    | Success (List CryptoInstrument)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getCryptoSymbolsList )


type alias CryptoInstrument =
    { symbol : String
    , name : String
    , currentPrice : Float
    }



-- UPDATE


type Msg
    = MorePlease
    | GotCryptoList (Result Http.Error (List CryptoInstrument))


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
        [ h2 [] [ text "Crypto instrument list. " ]
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

        Success cryptosList ->
            div []
                [ viewCryptos cryptosList ]


viewCryptos : List CryptoInstrument -> Html Msg
viewCryptos cryptoInstruments =
    div []
        [ h3 [] [ text "Cryptos" ]
        , table []
            ([ viewTableHeader ] ++ List.map viewCrypto cryptoInstruments)
        ]


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "Symbol" ]
        , th []
            [ text "Name" ]
        , th []
            [ text "Current Price" ]
        ]


viewCrypto : CryptoInstrument -> Html Msg
viewCrypto cryptoInstrument =
    tr []
        [ td []
            [ text cryptoInstrument.symbol ]
        , td []
            [ text cryptoInstrument.name ]
        , td []
            [ text ("$ " ++ String.fromFloat cryptoInstrument.currentPrice) ]
        ]



-- HTTP


getCryptoSymbolsList : Cmd Msg
getCryptoSymbolsList =
    Http.get
        { url = "https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd"
        , expect = Http.expectJson GotCryptoList (Decode.list cryptoInstrumentDecoder)
        }


cryptoInstrumentDecoder : Decoder CryptoInstrument
cryptoInstrumentDecoder =
    map3 CryptoInstrument
        (field "symbol" string)
        (field "name" string)
        (field "current_price" Decode.float)
