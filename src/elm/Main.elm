port module Main exposing (Model, Msg(..), initialModel, main)

import Accessibility.Aria as Aria
import Browser
import FontAwesome as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Html
    exposing
        ( Html
        , a
        , button
        , div
        , h1
        , img
        , main_
        , table
        , td
        , text
        , tr
        )
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import VitePluginHelper



-- CONSTANTS
-- MESSAGES


type Msg
    = ChangeTheme String
    | NoOp



-- MODEL


type alias Model =
    { theme : String
    }


type alias MenuOptionDetails =
    { title : String
    , description : String
    , href : String
    }



-- INIT


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { theme = "light"
      }
    , Cmd.none
    )



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- VIEW


view : Model -> Html Msg
view model =
    main_
        [ Aria.label "main content"
        , Attr.class "flex flex-col justify-center items-center min-w-full min-h-full font-sans text-center"
        ]
        [ menuOptionGroup
            "Canadian Immigration Tools"
            [ MenuOptionDetails "Proof of relationship" "Produce a document that helps prove your relationship to your sponsor." "/proof-of-relationship"
            , MenuOptionDetails "Proof of contact" "Produce a document that helps prove you have had contact with your sponsor." "/proof-of-contact"
            ]
        ]


menuOptionGroup : String -> List MenuOptionDetails -> Html Msg
menuOptionGroup title menuOptionDetails =
    div
        [ Attr.class "min-h-screen"
        ]
        [ h1 [] [ text title ]
        , div [] (List.map menuOption menuOptionDetails)
        ]


menuOption : MenuOptionDetails -> Html Msg
menuOption menuOptionDetails =
    a
        [ Attr.href menuOptionDetails.href
        ]
        [ div [] [ text menuOptionDetails.title ]
        , div [] [ text menuOptionDetails.description ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTheme string ->
            ( model
            , changeTheme string
            )

        NoOp ->
            ( model, Cmd.none )



-- COMMANDS
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- PORTS


port changeTheme : String -> Cmd msg
