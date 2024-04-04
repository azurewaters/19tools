port module Main exposing (Model, Msg(..), main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Home
import Html
    exposing
        ( Html
        , a
        , div
        , footer
        , header
        , hr
        , img
        , text
        )
import Html.Attributes exposing (alt, class, href, src, style)
import ProofOfContact
import ProofOfRelationshipPhotographs
import Url exposing (Url)
import Url.Parser exposing ((</>), s, string, top)



-- import VitePluginHelper
-- CONSTANTS
-- MODEL


type alias Model =
    { theme : String
    , page : Page
    , key : Nav.Key
    , url : Url.Url
    }


type Page
    = HomePage Home.Model
    | ProofOfRelationshipPhotographsPage ProofOfRelationshipPhotographs.Model
    | ProofOfContactPage ProofOfContact.Model
    | NotFoundPage


type Route
    = Home
    | ProofOfRelationship
    | ProofOfContact String



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    updateUrl url
        { theme = "light"
        , page = NotFoundPage
        , key = key
        , url = url
        }



-- MESSAGES


type Msg
    = ChangeTheme String
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotHomeMsg Home.Msg
    | GotProofOfRelationshipPhotographsMsg ProofOfRelationshipPhotographs.Msg
    | GotProofOfContactMsg ProofOfContact.Msg



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTheme string ->
            ( model
            , changeTheme string
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            updateUrl url model

        GotHomeMsg homeMsg ->
            case model.page of
                HomePage homeModel ->
                    Home.update homeMsg homeModel
                        |> toHome model

                _ ->
                    ( model, Cmd.none )

        GotProofOfRelationshipPhotographsMsg proofOfRelationshipHomeMsg ->
            case model.page of
                ProofOfRelationshipPhotographsPage proofOfRelationshipHomeModel ->
                    ProofOfRelationshipPhotographs.update proofOfRelationshipHomeMsg proofOfRelationshipHomeModel
                        |> toProofOfRelationship model

                _ ->
                    ( model, Cmd.none )

        GotProofOfContactMsg proofOfContactMsg ->
            case model.page of
                ProofOfContactPage proofOfContactModel ->
                    ProofOfContact.update proofOfContactMsg proofOfContactModel
                        |> toProofOfContact model

                _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    let
        content : Html Msg
        content =
            case model.page of
                HomePage homeModel ->
                    Home.view homeModel
                        |> Html.map GotHomeMsg

                ProofOfRelationshipPhotographsPage proofOfRelationshipHomeModel ->
                    ProofOfRelationshipPhotographs.view proofOfRelationshipHomeModel
                        |> Html.map GotProofOfRelationshipPhotographsMsg

                ProofOfContactPage proofOfContactModel ->
                    ProofOfContact.view proofOfContactModel
                        |> Html.map GotProofOfContactMsg

                NotFoundPage ->
                    div [] [ text "Not found" ]

        pageName =
            case model.page of
                HomePage _ ->
                    "home"

                ProofOfRelationshipPhotographsPage _ ->
                    "proof of relationship"

                ProofOfContactPage _ ->
                    "proof of contact"

                NotFoundPage ->
                    "not found"
    in
    { title = "19tools"
    , body =
        [ div
            [ class "flex flex-col gap-8 items-center min-h-screen"
            ]
            [ header
                [ class "p-8 flex flex-row items-baseline justify-between w-full border-b border-slate-200" ]
                [ div
                    [ class "flex flex-row gap-4 items-baseline" ]
                    [ a [ href "/", class "text-4xl font-bold tracking-tight" ] [ text "19tools" ]
                    , div [ class "text-2xl" ] [ text pageName ]
                    ]
                , div
                    [ class "flex flex-row gap-4 text-sm font-bold items-center" ]
                    [ a [ href "/login" ] [ text "log in" ]
                    , a [ href "/Sign up" ] [ text "sign up" ]
                    , div [ class "bg-gray-600 border border-gray-600 rounded-full h-12 w-12 hover:bg-gray-400" ] []
                    ]
                ]
            , content
            , footer
                [ class "mt-20 py-20 bg-slate-50 border-t border-slate-200 w-full text-slate-400" ]
                [ a [ href "https://www.buymeacoffee.com/azurewaters" ]
                    [ img
                        [ src "https://cdn.buymeacoffee.com/buttons/v2/default-yellow.png"
                        , alt "Buy Me A Coffee"
                        , style "height" "60px !important"
                        , style "width" "217px !important;"
                        , class "w-40 border border-gray-600 rounded-md mb-4"
                        ]
                        []
                    ]
                , div [] [ text "Need help? Write to 19tools@gmail.com" ]
                , div [] [ text "Copyright (C) 2024 Azurewaters Inc. All rights reserved. Terms and Conditions apply." ]
                ]
            ]
        ]
    }



-- COMMANDS
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        ProofOfRelationshipPhotographsPage proofOfRelationshipHomeModel ->
            ProofOfRelationshipPhotographs.subscriptions proofOfRelationshipHomeModel
                |> Sub.map GotProofOfRelationshipPhotographsMsg

        NotFoundPage ->
            Sub.none

        _ ->
            Sub.none



-- PORTS


port changeTheme : String -> Cmd msg



-- HELPER FUNCTIONS


urlParser : Url.Parser.Parser (Route -> a) a
urlParser =
    Url.Parser.oneOf
        [ Url.Parser.map Home top
        , Url.Parser.map ProofOfRelationship (s "proofofrelationship")
        , Url.Parser.map (ProofOfContact "") (s "proofofcontact")
        ]


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Url.Parser.parse urlParser url of
        Just Home ->
            toHome model Home.init

        Just ProofOfRelationship ->
            ProofOfRelationshipPhotographs.init ProofOfRelationshipPhotographs.initialModel
                |> toProofOfRelationship model

        Just (ProofOfContact _) ->
            toProofOfContact model ProofOfContact.init

        Nothing ->
            ( { model | page = NotFoundPage }, Cmd.none )


toHome : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
toHome model ( homeModel, homeCommand ) =
    ( { model | page = HomePage homeModel }
    , Cmd.map GotHomeMsg homeCommand
    )


toProofOfRelationship : Model -> ( ProofOfRelationshipPhotographs.Model, Cmd ProofOfRelationshipPhotographs.Msg ) -> ( Model, Cmd Msg )
toProofOfRelationship model ( proofOfRelationshipHomeModel, proofOfRelationshipHomeCommand ) =
    ( { model | page = ProofOfRelationshipPhotographsPage proofOfRelationshipHomeModel }
    , Cmd.map GotProofOfRelationshipPhotographsMsg proofOfRelationshipHomeCommand
    )


toProofOfContact : Model -> ( ProofOfContact.Model, Cmd ProofOfContact.Msg ) -> ( Model, Cmd Msg )
toProofOfContact model ( proofOfContactModel, proofofcontactCommand ) =
    ( { model | page = ProofOfContactPage proofOfContactModel }
    , Cmd.map GotProofOfContactMsg proofofcontactCommand
    )
