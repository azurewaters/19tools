module ProofOfRelationship exposing (..)

import File exposing (File)
import Html exposing (Html, button, div, h1, input, label, li, ol, p, text)
import Html.Attributes as Attr exposing (disabled, placeholder, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode exposing (Decoder)


type Screen
    = Home
    | CollectDetails
    | UploadPictures
    | Payment
    | DownloadDocuments


type alias UploadedPictureAndItsDescription =
    { id : String
    , position : Int
    , picture : File
    , description : String
    }


type alias Model =
    { -- General
      currentScreen : Screen

    -- Collect Details Screen
    , applicantsLastName : String
    , applicantsFirstName : String
    , sponsorsFullName : String
    , documentsName : String

    -- Upload Pictures Screen
    , proofOfRelationshipUploadedPicturesAndTheirDescriptions : List UploadedPictureAndItsDescription
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { currentScreen = Home
      , applicantsLastName = ""
      , applicantsFirstName = ""
      , sponsorsFullName = ""
      , documentsName = ""
      , proofOfRelationshipUploadedPicturesAndTheirDescriptions = []
      }
    , Cmd.none
    )



-- MESSAGE


type Msg
    = HomeNextClicked
      -- Collect Details
    | CollectDetailsApplicantsLastNameInputted String
    | CollectDetailsApplicantsFirstNameInputted String
    | CollectDetailsSponsorsNameInputted String
    | CollectDetailsDocumentNameInputted String
    | CollectDetailsPreviousClicked
    | CollectDetailsNextClicked
      -- Upload Pictures
    | UploadPicturesPicturesDragOver
    | UploadPicturesPicturesDropped
    | UploadPicturesAddPicturesClicked
    | UploadPicturesPreviousClicked
    | UploadPicturesNextClicked
      -- Payment
    | PaymentPreviousClicked
    | PaymentNextClicked
      -- Download Documents
    | DownloadDocumentsHomeClicked



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Home
        HomeNextClicked ->
            ( { model | currentScreen = CollectDetails }, Cmd.none )

        -- Collect Details
        CollectDetailsApplicantsLastNameInputted string ->
            ( { model | applicantsLastName = string }, Cmd.none )

        CollectDetailsApplicantsFirstNameInputted string ->
            ( { model | applicantsFirstName = string }, Cmd.none )

        CollectDetailsSponsorsNameInputted string ->
            ( { model | sponsorsFullName = string }, Cmd.none )

        CollectDetailsDocumentNameInputted string ->
            ( { model | documentsName = string }, Cmd.none )

        CollectDetailsPreviousClicked ->
            ( { model | currentScreen = Home }, Cmd.none )

        CollectDetailsNextClicked ->
            ( { model | currentScreen = UploadPictures }, Cmd.none )

        -- Upload Pictures
        UploadPicturesPicturesDragOver ->
            ( model, Cmd.none )

        UploadPicturesPicturesDropped ->
            ( model, Cmd.none )

        UploadPicturesAddPicturesClicked ->
            ( model, Cmd.none )

        UploadPicturesPreviousClicked ->
            ( { model | currentScreen = CollectDetails }, Cmd.none )

        UploadPicturesNextClicked ->
            ( { model | currentScreen = Payment }, Cmd.none )

        -- Payment
        PaymentPreviousClicked ->
            ( { model | currentScreen = UploadPictures }, Cmd.none )

        PaymentNextClicked ->
            ( { model | currentScreen = DownloadDocuments }, Cmd.none )

        -- Download Documents
        DownloadDocumentsHomeClicked ->
            ( { model | currentScreen = Home }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ Attr.class "container mx-auto flex flex-col gap-4 mt-8"
        ]
        [ h1
            [ Attr.class "text-2xl font-bold" ]
            [ text "Proof of relationship" ]
        , div
            [ Attr.class "flex flex-col items-center" ]
            [ text "Proof of relationship"
            , case model.currentScreen of
                Home ->
                    home model

                CollectDetails ->
                    collectDetails model

                UploadPictures ->
                    uploadPictures model

                Payment ->
                    payment model

                DownloadDocuments ->
                    downloadDocuments model
            ]
        ]


home : Model -> Html Msg
home _ =
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
        , button [ onClick HomeNextClicked ] [ text "Next" ]
        ]


collectDetails : Model -> Html Msg
collectDetails model =
    div
        [ Attr.class "flex flex-col gap-2" ]
        [ h1 [] [ text "Collect details" ]
        , div
            []
            [ label [] [ text "The applicant's last name" ]
            , input [ type_ "text", placeholder "The applicant's last name", value model.applicantsLastName, onInput CollectDetailsApplicantsLastNameInputted ] []
            ]
        , div
            []
            [ label [] [ text "The applicant's first name" ]
            , input [ type_ "text", placeholder "The applicant's first name", value model.applicantsFirstName, onInput CollectDetailsApplicantsFirstNameInputted ] []
            ]
        , div
            []
            [ label [] [ text "The sponsor's full name" ]
            , input [ type_ "text", placeholder "The sponsor's full name", value model.sponsorsFullName, onInput CollectDetailsSponsorsNameInputted ] []
            ]
        , div
            []
            [ label [] [ text "The document's name" ]
            , input [ type_ "text", placeholder "The document's name", value model.documentsName, onInput CollectDetailsDocumentNameInputted ] []
            ]
        , div
            []
            [ button [ onClick CollectDetailsPreviousClicked ] [ text "Previous" ]
            , button [ onClick CollectDetailsNextClicked ] [ text "Next" ]
            ]
        ]


uploadPictures : Model -> Html Msg
uploadPictures model =
    div
        [ Attr.class "flex flex-col gap-2" ]
        [ h1 [] [ text "Upload pictures" ]
        , div
            [ on "dragover" uploadPicturesPicturesDragOverDecoder
            , on "drop" uploadPicturesPicturesDroppedDecoder
            ]
            [ div [ Attr.class "text-gray-100", disabled True ] [ text "Drag and drop pictures here" ]
            , button [ onClick UploadPicturesAddPicturesClicked ] [ text "Add pictures" ]
            ]
        , div []
            [ button [ onClick UploadPicturesPreviousClicked ] [ text "Previous" ]
            , button [ onClick UploadPicturesNextClicked ] [ text "Next" ]
            ]
        ]


payment : Model -> Html Msg
payment _ =
    div
        [ Attr.class "flex flex-col gap-2" ]
        [ h1 [] [ text "Payment" ]
        , p [] [ text "You will be charged $10.00 for this service." ]
        , button [ onClick PaymentPreviousClicked ] [ text "Previous" ]
        , button [ onClick PaymentNextClicked ] [ text "Next" ]
        ]


downloadDocuments : Model -> Html Msg
downloadDocuments _ =
    div
        [ Attr.class "flex flex-col gap-2" ]
        [ h1 [] [ text "Download documents" ]
        , p [] [ text "You can download the document you created." ]
        , button [ onClick DownloadDocumentsHomeClicked ] [ text "Home" ]
        ]



-- DECODERS


uploadPicturesPicturesDragOverDecoder : Decoder Msg
uploadPicturesPicturesDragOverDecoder =
    Decode.succeed UploadPicturesPicturesDragOver


uploadPicturesPicturesDroppedDecoder : Decoder Msg
uploadPicturesPicturesDroppedDecoder =
    Decode.succeed UploadPicturesPicturesDropped
