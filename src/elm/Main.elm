port module Main exposing (Model, Msg(..), main)

import Accessibility.Aria as Aria
import Browser
import Browser.Navigation as Nav
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
        , input
        , label
        , li
        , main_
        , ol
        , p
        , text
        )
import Html.Attributes as Attr exposing (disabled, href, placeholder, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Url exposing (Url)
import VitePluginHelper



-- CONSTANTS
-- MESSAGES


type Msg
    = ChangeTheme String
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | MenuOptionProofOfRelationshipClicked
    | MenuOptionProofOfContactClicked
      --  Proof of Relationship
      -- Proof of Relationship - Home
    | ProofOfRelationshipCollectDetailsHomeNextClicked
      -- Proof of Relationship - Collect Details
    | ProofOfRelationshipCollectDetailsApplicantsLastNameInputted String
    | ProofOfRelationshipCollectDetailsApplicantsFirstNameInputted String
    | ProofOfRelationshipCollectDetailsSponsorsNameInputted String
    | ProofOfRelationshipCollectDetailsDocumentNameInputted String
    | ProofOfRelationshipCollectDetailsPreviousClicked
    | ProofOfRelationshipCollectDetailsNextClicked
      -- Proof of Relationship - Upload Pictures
    | ProofOfRelationshipUploadPicturesPicturesDragOver
    | ProofOfRelationshipUploadPicturesPicturesDropped
    | ProofOfRelationshipUploadPicturesAddPicturesClicked
    | ProofOfRelationshipUploadPicturesPreviousClicked
    | ProofOfRelationshipUploadPicturesNextClicked
      -- Other
    | NoOp



-- MODEL


type alias Model =
    { theme : String
    , key : Nav.Key
    , url : Url.Url

    -- Proof of Relationship
    , applicantsLastName : String
    , applicantsFirstName : String
    , sponsorsFullName : String
    , documentsName : String
    }


type alias MenuOptionDetails =
    { title : String
    , description : String
    , msg : Msg
    }



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
    ( { theme = "light"
      , key = key
      , url = url
      , applicantsLastName = ""
      , applicantsFirstName = ""
      , sponsorsFullName = ""
      , documentsName = ""
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTheme string ->
            ( model
            , changeTheme string
            )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        MenuOptionProofOfRelationshipClicked ->
            ( model, Nav.pushUrl model.key "/proof-of-relationship" )

        MenuOptionProofOfContactClicked ->
            ( model, Nav.pushUrl model.key "/proof-of-contact" )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "19tools"
    , body =
        [ div
            [ Aria.label "main content"
            , Attr.class "flex flex-col justify-center items-start min-w-full min-h-full"
            ]
            [ header_
            , case model.url.path of
                "/proof-of-relationship" ->
                    proofOfRelationship model

                "/proof-of-contact" ->
                    proofOfContact model

                _ ->
                    home model
            ]
        ]
    }


header_ : Html Msg
header_ =
    header [ Attr.class "text-lg p-4" ] [ a [ href "/" ] [ text "19tools" ] ]



-- VIEW: Home


home : Model -> Html Msg
home _ =
    div
        [ Attr.class "container mx-auto flex flex-col gap-4 mt-8"
        ]
        [ menuOptionGroup
            "Canadian Immigration Tools"
            [ MenuOptionDetails "Proof of relationship" "Produce a document that helps prove your relationship to your sponsor." MenuOptionProofOfRelationshipClicked
            , MenuOptionDetails "Proof of contact" "Produce a document that helps prove you have had contact with your sponsor." MenuOptionProofOfContactClicked
            ]
        ]


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
    button
        [ Attr.class "flex flex-col border border-slate-200 rounded-md p-4 hover:bg-slate-100"
        , onClick menuOptionDetails.msg
        ]
        [ div [ Attr.class "text-xl font-bold" ] [ text menuOptionDetails.title ]
        , div [] [ text menuOptionDetails.description ]
        ]



-- VIEW: Proof of Relationship


proofOfRelationship : Model -> Html Msg
proofOfRelationship model =
    div
        [ Attr.class "container mx-auto flex flex-col gap-4 mt-8"
        ]
        [ h1
            [ Attr.class "text-2xl font-bold" ]
            [ text "Proof of relationship" ]
        , div
            [ Attr.class "flex flex-col items-center" ]
            [ text "Proof of relationship"
            , case model.url.path of
                "/proof-of-relationship" ->
                    proofOfRelationshipHome model

                "/proof-of-relationship/collect-details" ->
                    proofOfRelationshipCollectDetails model

                _ ->
                    proofOfRelationshipHome model
            ]
        ]


proofOfRelationshipHome : Model -> Html Msg
proofOfRelationshipHome _ =
    div
        [ Attr.class "flex flex-col gap-2" ]
        [ h1 [] [ text "Overview" ]
        , p [] [ text "You’ll be led through the following steps to produce a document to help prove your relationship:" ]
        , ol
            [ Attr.class "flex flex-col gap-2" ]
            [ li [] [ text "Type in your and your sponsor’s details." ]
            , li [] [ text "Upload pictures and their descriptions." ]
            , li [] [ text "Make your payment." ]
            , li [] [ text "Download one or more documents." ]
            ]
        , button [ onClick ProofOfRelationshipCollectDetailsHomeNextClicked ] [ text "Next" ]
        ]


proofOfRelationshipCollectDetails : Model -> Html Msg
proofOfRelationshipCollectDetails model =
    div
        [ Attr.class "flex flex-col gap-2" ]
        [ h1 [] [ text "Collect details" ]
        , div
            []
            [ label [] [ text "The applicant's last name" ]
            , input [ type_ "text", placeholder "The applicant's last name", value model.applicantsLastName, onInput ProofOfRelationshipCollectDetailsApplicantsLastNameInputted ] []
            ]
        , div
            []
            [ label [] [ text "The applicant's first name" ]
            , input [ type_ "text", placeholder "The applicant's first name", value model.applicantsFirstName, onInput ProofOfRelationshipCollectDetailsApplicantsFirstNameInputted ] []
            ]
        , div
            []
            [ label [] [ text "The sponsor's full name" ]
            , input [ type_ "text", placeholder "The sponsor's full name", value model.sponsorsFullName, onInput ProofOfRelationshipCollectDetailsSponsorsNameInputted ] []
            ]
        , div
            []
            [ label [] [ text "The document's name" ]
            , input [ type_ "text", placeholder "The document's name", value model.documentsName, onInput ProofOfRelationshipCollectDetailsDocumentNameInputted ] []
            ]
        , div
            []
            [ button [ onClick ProofOfRelationshipCollectDetailsPreviousClicked ] [ text "Previous" ]
            , button [ onClick ProofOfRelationshipCollectDetailsNextClicked ] [ text "Next" ]
            ]
        ]


proofOfRelationshipUploadPictures : Model -> Html Msg
proofOfRelationshipUploadPictures model =
    div
        [ Attr.class "flex flex-col gap-2" ]
        [ h1 [] [ text "Upload pictures" ]
        , div
            [ on "dragover" proofOfRelationshipUploadPicturesPicturesDragOverDecoder
            , on "drop" proofOfRelationshipUploadPicturesPicturesDroppedDecoder
            ]
            [ div [ Attr.class "text-gray-100", disabled True ] [ text "Drag and drop pictures here" ]
            , button [ onClick ProofOfRelationshipUploadPicturesAddPicturesClicked ] [ text "Add pictures" ]
            ]
        , div []
            [ button [ onClick ProofOfRelationshipUploadPicturesPreviousClicked ] [ text "Previous" ]
            , button [ onClick ProofOfRelationshipUploadPicturesNextClicked ] [ text "Next" ]
            ]
        ]



-- VIEW: Proof of Contact


proofOfContact : Model -> Html Msg
proofOfContact _ =
    div
        [ Attr.class "container mx-auto flex flex-col gap-4 mt-8"
        ]
        [ h1 [ Attr.class "text-2xl font-bold" ] [ text "Proof of contact" ]
        , div [ Attr.class "flex flex-col gap-2" ]
            [ text "Proof of contact"
            ]
        ]



-- COMMANDS
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- PORTS


port changeTheme : String -> Cmd msg



-- DECODERS


proofOfRelationshipUploadPicturesPicturesDragOverDecoder : Decoder Msg
proofOfRelationshipUploadPicturesPicturesDragOverDecoder =
    Decode.succeed ProofOfRelationshipUploadPicturesPicturesDragOver

proofOfRelationshipUploadPicturesPicturesDroppedDecoder : Decoder Msg
proofOfRelationshipUploadPicturesPicturesDroppedDecoder =
    Decode.succeed ProofOfRelationshipUploadPicturesPicturesDropped
