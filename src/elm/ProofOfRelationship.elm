port module ProofOfRelationship exposing (Model, Msg, init, initialModel, subscriptions, update, view)

import File exposing (File)
import File.Download as Download
import File.Select
import Html exposing (Html, button, div, h1, input, label, p, text)
import Html.Attributes as Attr exposing (disabled, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import Json.Decode as Decode
import Json.Encode as Encode
import Task


type alias Picture =
    { id : Int
    , position : Int
    , picture : File
    , contentInURLFormat : String
    , description : String
    }



-- MODEL


type alias Model =
    { lastIdUsed : Int
    , pictures : List Picture
    , documentsName : String
    , pdfInURLFormat : String
    }


initialModel : Model
initialModel =
    { lastIdUsed = 0
    , pictures = []
    , documentsName = ""
    , pdfInURLFormat = ""
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
    | DocumentNameInputted String
      -- Upload Pictures
    | PicturesDropped (List File)
    | PicturesSelected File (List File)
    | AddPicturesClicked
    | GotPictureInUrlFormat Int (Result String String)
    | PicturesDescriptionInputted Int String
    | DeletePictureClicked Int
      -- Download Documents
    | DownloadDocumentsClicked
    | GotThePDF String
      -- Other
    | NoOp



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Home
        HomeNextClicked ->
            ( model, Cmd.none )

        --  Collect Details
        DocumentNameInputted documentsName ->
            ( { model | documentsName = documentsName }, Cmd.none )

        -- Upload Pictures
        PicturesDropped files ->
            let
                -- Filter out the files that are already uploaded
                -- Make a PictureAndItsDescription for each file with its id set to the next id
                newPicturesAndTheirDescriptions =
                    files
                        |> getOnlyNewFiles model.pictures
                        |> makePicturesAndTheirDescriptions model.lastIdUsed
            in
            ( { model
                | lastIdUsed = model.lastIdUsed + List.length newPicturesAndTheirDescriptions
                , pictures = model.pictures ++ newPicturesAndTheirDescriptions
              }
            , Cmd.batch (getPicturesInUrlFormatCmds newPicturesAndTheirDescriptions)
            )

        PicturesSelected file files ->
            let
                newPicturesAndTheirDescriptions =
                    (file :: files)
                        |> getOnlyNewFiles model.pictures
                        |> makePicturesAndTheirDescriptions model.lastIdUsed
            in
            ( { model
                | lastIdUsed = model.lastIdUsed + List.length newPicturesAndTheirDescriptions
                , pictures = model.pictures ++ newPicturesAndTheirDescriptions
              }
            , Cmd.batch (getPicturesInUrlFormatCmds newPicturesAndTheirDescriptions)
            )

        AddPicturesClicked ->
            ( model, File.Select.files [ "image/png", "image/jpg", "image/jpeg" ] PicturesSelected )

        GotPictureInUrlFormat id result ->
            case result of
                Ok pictureInUrlFormat ->
                    let
                        updatedPicturesAndTheirDescriptions : List Picture
                        updatedPicturesAndTheirDescriptions =
                            List.map
                                (\pictureAndItsDescription ->
                                    if pictureAndItsDescription.id == id then
                                        { pictureAndItsDescription | contentInURLFormat = pictureInUrlFormat }

                                    else
                                        pictureAndItsDescription
                                )
                                model.pictures
                    in
                    ( { model | pictures = updatedPicturesAndTheirDescriptions }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        PicturesDescriptionInputted id description ->
            ( { model
                | pictures =
                    List.map
                        (\pictureAndItsDescription ->
                            if pictureAndItsDescription.id == id then
                                { pictureAndItsDescription | description = description }

                            else
                                pictureAndItsDescription
                        )
                        model.pictures
              }
            , Cmd.none
            )

        DeletePictureClicked id ->
            ( { model
                | pictures =
                    List.filter
                        (\pictureAndItsDescription ->
                            pictureAndItsDescription.id /= id
                        )
                        model.pictures
              }
            , Cmd.none
            )

        -- Download Documents
        DownloadDocumentsClicked ->
            ( model, renderThePDF (getEncodedDocumentDefinition model.pictures) )

        GotThePDF pdfInURLFormat ->
            -- We just received the prepared PDF
            -- Download it to the user's computer
            ( { model | pdfInURLFormat = pdfInURLFormat }
            , Download.string model.documentsName pdfInURLFormat "application/pdf"
            )

        -- Other
        NoOp ->
            ( model, Cmd.none )



-- PORTS


port renderThePDF : Encode.Value -> Cmd msg



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
            [ Attr.class "flex flex-col gap-2" ]
            [ p [] [ text "You’ll be led through the following steps to produce a document to help prove your relationship:" ]

            -- Collect Details
            , div
                [ Attr.class "flex flex-col gap-2" ]
                [ label [] [ text "The document's name" ]
                , input [ type_ "text", placeholder "The document's name", value model.documentsName, onInput DocumentNameInputted ] []
                ]

            -- Upload Pictures
            , div
                [ Attr.class "flex flex-col gap-2" ]
                [ h1 [] [ text "Pictures" ]
                , case model.pictures of
                    [] ->
                        dropZone

                    _ ->
                        picturesList model.pictures
                ]

            -- Download Documents
            , div
                [ Attr.class "flex justify-end p-4" ]
                [ button [ onClick DownloadDocumentsClicked ] [ text "Download Documents" ]
                ]
            ]
        ]


picturesList : List Picture -> Html Msg
picturesList picturesAndTheirDescriptions =
    div
        [ Attr.class "p-8 border border-grey-300 rounded flex gap-8 flex-wrap mx-auto"
        , onFilesDrop PicturesDropped
        , onDragOver NoOp
        ]
        (List.map picture picturesAndTheirDescriptions)


dropZone : Html Msg
dropZone =
    div
        [ Attr.class "flex flex-col gap-8 items-center justify-center p-8 w-full border-2 border-gray-100 hover:border-gray-200 hover:bg-gray-50 rounded"
        , onFilesDrop PicturesDropped
        , onDragOver NoOp
        ]
        [ div [ Attr.class "text-gray-300", disabled True ] [ text "Drag and drop pictures here" ]
        , button
            [ onClick AddPicturesClicked
            , Attr.class "bg-gray-900 border border-gray-900 rounded text-gray-200 hover:text-gray-100 text-sm font-semibold hover:bg-black px-4 py-2"
            ]
            [ text "Add pictures" ]
        ]


picture : Picture -> Html Msg
picture pictureAndItsDescription =
    div
        [ Attr.class "group flex flex-col gap-2" ]
        [ div
            [ Attr.class "relative bg-no-repeat bg-contain bg-center h-64 w-full"
            , Attr.style "background-image" ("url('" ++ pictureAndItsDescription.contentInURLFormat ++ "')")
            ]
            [ div
                [ Attr.class "invisible group-hover:visible absolute right-2 top-2 rounded-full bg-gray-100 border border-gray-200 p-2 hover:bg-gray-200"
                , onClick (DeletePictureClicked pictureAndItsDescription.id)
                ]
                [ text "x" ]
            ]
        , input
            [ type_ "text"
            , placeholder "Description"
            , value pictureAndItsDescription.description
            , onInput (PicturesDescriptionInputted pictureAndItsDescription.id)
            , Attr.class "group-focus:border group-focus:border-gray-300 rounded placeholder:text-gray-300"
            ]
            []
        ]



-- SUBSCRIPTIONS


port receiveThePDF : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveThePDF GotThePDF
        ]



-- EVENTS


onDragOver : Msg -> Html.Attribute Msg
onDragOver msg =
    preventDefaultOn "dragover" (Decode.succeed ( msg, True ))


onFilesDrop : (List File -> Msg) -> Html.Attribute Msg
onFilesDrop msg =
    preventDefaultOn "drop" (Decode.map2 Tuple.pair (Decode.map msg filesDecoder) (Decode.succeed True))



-- DECODERS


filesDecoder : Decode.Decoder (List File)
filesDecoder =
    Decode.at [ "dataTransfer", "files" ] (Decode.list File.decoder)



-- OTHER HELPER FUNCTIONS


getPictureInUrlFormat : Int -> File -> Cmd Msg
getPictureInUrlFormat id file =
    Task.attempt (GotPictureInUrlFormat id) (File.toUrl file)


getOnlyNewFiles : List Picture -> List File -> List File
getOnlyNewFiles picturesAndTheirDescriptions files =
    List.filter
        (\file ->
            List.all
                (\p ->
                    File.name p.picture /= File.name file
                )
                picturesAndTheirDescriptions
        )
        files


makePicturesAndTheirDescriptions : Int -> List File -> List Picture
makePicturesAndTheirDescriptions lastIdUsed files =
    List.indexedMap
        (\index file ->
            let
                newId =
                    lastIdUsed + index + 1
            in
            { id = newId
            , position = newId
            , picture = file
            , contentInURLFormat = ""
            , description = ""
            }
        )
        files


getPicturesInUrlFormatCmds : List Picture -> List (Cmd Msg)
getPicturesInUrlFormatCmds ps =
    List.map
        (\p ->
            getPictureInUrlFormat p.id p.picture
        )
        ps



-- PDF Document Description helpers


getEncodedDocumentDefinition : List Picture -> Encode.Value
getEncodedDocumentDefinition ps =
    -- This is where we combine the TitlePage and the PicturePages and put it together into an encoded object called "content" and send that out
    Encode.object
        [ ( "content", Encode.list identity (List.concat [ getEncodedTitlePage, getEncodedPicturePages ps ]) )
        ]


getEncodedTitlePage : List Encode.Value
getEncodedTitlePage =
    [ Encode.object
        [ ( "text", Encode.string "Proof of Relationship" )
        , ( "fontSize", Encode.int 24 )
        , ( "bold", Encode.bool True )
        ]
    , Encode.object
        [ ( "text", Encode.string "This document contains pictures that establish my relationship with my sponsor." )
        , ( "pageBreak", Encode.string "after" )
        ]
    ]


getEncodedPicturePages : List Picture -> List Encode.Value
getEncodedPicturePages ps =
    List.map getEncodedPicturePage ps
        |> List.concat


getEncodedPicturePage : Picture -> List Encode.Value
getEncodedPicturePage p =
    [ getEncodedPicture p.contentInURLFormat
    , getEncodedPictureDescription p.description
    ]


getEncodedPicture : String -> Encode.Value
getEncodedPicture contentInURLFormat =
    Encode.object
        [ ( "image", Encode.string contentInURLFormat )
        ]


getEncodedPictureDescription : String -> Encode.Value
getEncodedPictureDescription description =
    Encode.object
        [ ( "text", Encode.string description )
        , ( "fontSize", Encode.int 12 )
        , ( "pageBreak", Encode.string "after" )
        ]
