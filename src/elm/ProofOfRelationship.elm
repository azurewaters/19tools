module ProofOfRelationship exposing (Model, Msg, init, initialModel, update, view)

import File exposing (File)
import Html exposing (Html, button, div, h1, input, label, li, ol, p, text)
import Html.Attributes as Attr exposing (disabled, placeholder, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode exposing (Decoder)


type alias UploadedPictureAndItsDescription =
    { id : String
    , position : Int
    , picture : File
    , description : String
    }


type alias Model =
    { applicantsLastName : String
    , applicantsFirstName : String
    , sponsorsFullName : String
    , documentsName : String
    , proofOfRelationshipUploadedPicturesAndTheirDescriptions : List UploadedPictureAndItsDescription
    }


initialModel : Model
initialModel =
    { applicantsLastName = ""
    , applicantsFirstName = ""
    , sponsorsFullName = ""
    , documentsName = ""
    , proofOfRelationshipUploadedPicturesAndTheirDescriptions = []
    }



-- INIT


init : Model -> ( Model, Cmd Msg )
init model =
    ( model
    , Cmd.none
    )



-- MESSAGE


type Msg
    = HomeNextClicked
      -- Collect Details
    | ApplicantsLastNameInputted String
    | ApplicantsFirstNameInputted String
    | SponsorsNameInputted String
    | DocumentNameInputted String
      -- Upload Pictures
    | PicturesDragOver
    | PicturesDropped
    | AddPicturesClicked
    | PreviousClicked
      -- Download Documents
    | DownloadDocumentsClicked



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Home
        HomeNextClicked ->
            ( model, Cmd.none )

        --  Collect Details
        ApplicantsLastNameInputted applicantsLastName ->
            ( { model | applicantsLastName = applicantsLastName }, Cmd.none )

        ApplicantsFirstNameInputted applicantsFirstName ->
            ( { model | applicantsFirstName = applicantsFirstName }, Cmd.none )

        SponsorsNameInputted sponsorsFullName ->
            ( { model | sponsorsFullName = sponsorsFullName }, Cmd.none )

        DocumentNameInputted documentsName ->
            ( { model | documentsName = documentsName }, Cmd.none )

        -- Upload Pictures
        PicturesDragOver ->
            ( model, Cmd.none )

        PicturesDropped ->
            ( model, Cmd.none )

        AddPicturesClicked ->
            ( model, Cmd.none )

        PreviousClicked ->
            ( model, Cmd.none )

        -- Download Documents
        DownloadDocumentsClicked ->
            ( model, Cmd.none )



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
            , div
                [ Attr.class "flex flex-col gap-2" ]
                [ h1 [] [ text "Overview" ]
                , p [] [ text "You’ll be led through the following steps to produce a document to help prove your relationship:" ]

                -- Collect Details
                , div
                    [ Attr.class "flex flex-col gap-2" ]
                    [ h1 [] [ text "Collect details" ]
                    , div
                        []
                        [ label [] [ text "The applicant's last name" ]
                        , input [ type_ "text", placeholder "The applicant's last name", value model.applicantsLastName, onInput ApplicantsLastNameInputted ] []
                        ]
                    , div
                        []
                        [ label [] [ text "The applicant's first name" ]
                        , input [ type_ "text", placeholder "The applicant's first name", value model.applicantsFirstName, onInput ApplicantsFirstNameInputted ] []
                        ]
                    , div
                        []
                        [ label [] [ text "The sponsor's full name" ]
                        , input [ type_ "text", placeholder "The sponsor's full name", value model.sponsorsFullName, onInput SponsorsNameInputted ] []
                        ]
                    , div
                        []
                        [ label [] [ text "The document's name" ]
                        , input [ type_ "text", placeholder "The document's name", value model.documentsName, onInput DocumentNameInputted ] []
                        ]
                    ]

                -- Upload Pictures
                , div
                    [ Attr.class "flex flex-col gap-2" ]
                    [ h1 [] [ text "Upload pictures" ]
                    , div
                        [ on "dragover" picturesDragOverDecoder
                        , on "drop" picturesDroppedDecoder
                        ]
                        [ div [ Attr.class "text-gray-100", disabled True ] [ text "Drag and drop pictures here" ]
                        , button [ onClick AddPicturesClicked ] [ text "Add pictures" ]
                        ]
                    ]
                ]
            ]
        ]



-- uploadPictures : Model -> Html Msg
-- uploadPictures model =
-- payment : Model -> Html Msg
-- payment _ =
--     div
--         [ Attr.class "flex flex-col gap-2" ]
--         [ h1 [] [ text "Payment" ]
--         , p [] [ text "You will be charged $10.00 for this service." ]
--         , button [ onClick PaymentPreviousClicked ] [ text "Previous" ]
--         , button [ onClick PaymentNextClicked ] [ text "Next" ]
--         ]
-- downloadDocuments : Model -> Html Msg
-- downloadDocuments _ =
--     div
--         [ Attr.class "flex flex-col gap-2" ]
--         [ h1 [] [ text "Download documents" ]
--         , p [] [ text "You can download the document you created." ]
--         , button [ onClick DownloadDocumentsHomeClicked ] [ text "Home" ]
--         ]
-- DECODERS


picturesDragOverDecoder : Decoder Msg
picturesDragOverDecoder =
    Decode.succeed PicturesDragOver


picturesDroppedDecoder : Decoder Msg
picturesDroppedDecoder =
    Decode.succeed PicturesDropped
