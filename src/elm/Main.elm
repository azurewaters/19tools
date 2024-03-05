port module Main exposing (Model, Msg(..), main)

import Accessibility.Aria as Aria
import Browser exposing (Document)
import Browser.Navigation as Nav
import Home
import Html
    exposing
        ( Html
        , a
        , div
        , header
        , text
        )
import Html.Attributes as Attr exposing (href)
import ProofOfContact
import ProofOfRelationship
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
    | ProofOfRelationshipPage ProofOfRelationship.Model
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
    | GotProofOfRelationshipHomeMsg ProofOfRelationship.Msg
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

        GotProofOfRelationshipHomeMsg proofOfRelationshipHomeMsg ->
            case model.page of
                ProofOfRelationshipPage proofOfRelationshipHomeModel ->
                    ProofOfRelationship.update proofOfRelationshipHomeMsg proofOfRelationshipHomeModel
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

                ProofOfRelationshipPage proofOfRelationshipHomeModel ->
                    ProofOfRelationship.view proofOfRelationshipHomeModel
                        |> Html.map GotProofOfRelationshipHomeMsg

                ProofOfContactPage proofOfContactModel ->
                    ProofOfContact.view proofOfContactModel
                        |> Html.map GotProofOfContactMsg

                NotFoundPage ->
                    div [] [ text "Not found" ]
    in
    { title = "19tools"
    , body =
        [ div
            [ Aria.label "main content"
            , Attr.class "flex flex-col justify-center items-start min-w-full min-h-full"
            ]
            [ header_
            , content
            , footer
            ]
        ]
    }


header_ : Html Msg
header_ =
    header [ Attr.class "text-lg p-4" ] [ a [ href "/" ] [ text "19tools" ] ]


footer : Html Msg
footer =
    div [ Attr.class "flex items-center text-sm p-4" ] [ text "© 2024 19tools" ]



-- COMMANDS
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        ProofOfRelationshipPage proofOfRelationshipHomeModel ->
            ProofOfRelationship.subscriptions proofOfRelationshipHomeModel
                |> Sub.map GotProofOfRelationshipHomeMsg

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
        , Url.Parser.map (ProofOfContact "") (s "canadianimmigration" </> s "proofofcontact")
        ]


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Url.Parser.parse urlParser url of
        Just Home ->
            toHome model Home.init

        Just ProofOfRelationship ->
            ProofOfRelationship.init ProofOfRelationship.initialModel
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


toProofOfRelationship : Model -> ( ProofOfRelationship.Model, Cmd ProofOfRelationship.Msg ) -> ( Model, Cmd Msg )
toProofOfRelationship model ( proofOfRelationshipHomeModel, proofOfRelationshipHomeCommand ) =
    ( { model | page = ProofOfRelationshipPage proofOfRelationshipHomeModel }
    , Cmd.map GotProofOfRelationshipHomeMsg proofOfRelationshipHomeCommand
    )


toProofOfContact : Model -> ( ProofOfContact.Model, Cmd ProofOfContact.Msg ) -> ( Model, Cmd Msg )
toProofOfContact model ( proofOfContactModel, proofofcontactCommand ) =
    ( { model | page = ProofOfContactPage proofOfContactModel }
    , Cmd.map GotProofOfContactMsg proofofcontactCommand
    )
