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
        , header
        , img
        , main_
        , table
        , td
        , text
        , tr
        )
import Html.Attributes as Attr exposing (href)
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
        , Attr.class "flex flex-col justify-center items-start min-w-full min-h-full font-sans"
        ]
        [ header_
        , div
            [ Attr.class "container mx-auto flex flex-col gap-4 mt-8"
            ]
            [ menuOptionGroup
                "Canadian Immigration Tools"
                [ MenuOptionDetails "Proof of relationship" "Produce a document that helps prove your relationship to your sponsor." "/proof-of-relationship"
                , MenuOptionDetails "Proof of contact" "Produce a document that helps prove you have had contact with your sponsor." "/proof-of-contact"
                ]
            ]
        ]


header_ : Html Msg
header_ =
    header [ Attr.class "text-lg" ] [ a [ href "/" ] [ text "19tools" ] ]


menuOptionGroup : String -> List MenuOptionDetails -> Html Msg
menuOptionGroup title menuOptionDetails =
    div
        [ Attr.class "flex flex-col gap-2"
        ]
        [ h1 [ Attr.class "text-2xl font-bold" ] [ text title ]
        , div [ Attr.class "flex flex-col gap-2" ] (List.map menuOption menuOptionDetails)
        ]


menuOption : MenuOptionDetails -> Html Msg
menuOption menuOptionDetails =
    a
        [ Attr.href menuOptionDetails.href
        , Attr.class "flex flex-col border border-slate-200 rounded-md p-4 hover:bg-slate-100"
        ]
        [ div [ Attr.class "text-xl font-bold" ] [ text menuOptionDetails.title ]
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
