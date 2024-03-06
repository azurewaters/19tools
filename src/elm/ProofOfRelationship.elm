port module ProofOfRelationship exposing (Model, Msg, init, initialModel, subscriptions, update, view)

import File exposing (File)
import File.Download as Download
import File.Select
import Html exposing (Html, button, div, h1, input, label, p, text)
import Html.Attributes as Attr exposing (disabled, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import Json.Decode as Decode
import Json.Encode as Encode
import ProofOfContact exposing (Model)
import Task


type alias Picture =
    { position : Int
    , picture : File
    , contentInURLFormat : String
    , description : String
    }



-- MODEL


type alias Model =
    { pictures : List Picture
    , documentsName : String
    , pictureBeingDragged : Maybe Picture
    , pictureBeingDraggedOver : Maybe Picture
    }


initialModel : Model
initialModel =
    { pictures = []
    , documentsName = ""
    , pictureBeingDragged = Nothing
    , pictureBeingDraggedOver = Nothing
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
    | GotPictureInUrlFormat String (Result String String)
    | PicturesDescriptionInputted String String
    | DeletePictureClicked String
    | PictureDragStarted Picture
    | PictureDraggedOver Picture
    | PictureDragEnded
    | PictureDroppedOn Picture
      -- Download Documents
    | DownloadDocumentsClicked
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
                        |> makePictures (List.length model.pictures)
            in
            ( { model
                | pictures = model.pictures ++ newPicturesAndTheirDescriptions
              }
            , Cmd.batch (getPicturesInUrlFormatCmds newPicturesAndTheirDescriptions)
            )

        PicturesSelected file files ->
            let
                newPicturesAndTheirDescriptions =
                    (file :: files)
                        |> getOnlyNewFiles model.pictures
                        |> makePictures (List.length model.pictures)
            in
            ( { model
                | pictures = model.pictures ++ newPicturesAndTheirDescriptions
              }
            , Cmd.batch (getPicturesInUrlFormatCmds newPicturesAndTheirDescriptions)
            )

        AddPicturesClicked ->
            ( model, File.Select.files [ "image/png", "image/jpg", "image/jpeg" ] PicturesSelected )

        GotPictureInUrlFormat picturesName result ->
            case result of
                Ok pictureInUrlFormat ->
                    let
                        updatedPicturesAndTheirDescriptions : List Picture
                        updatedPicturesAndTheirDescriptions =
                            List.map
                                (\pictureAndItsDescription ->
                                    if File.name pictureAndItsDescription.picture == picturesName then
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

        PicturesDescriptionInputted name description ->
            ( { model
                | pictures =
                    List.map
                        (\p ->
                            if File.name p.picture == name then
                                { p | description = description }

                            else
                                p
                        )
                        model.pictures
              }
            , Cmd.none
            )

        DeletePictureClicked picturesName ->
            ( { model
                | pictures =
                    List.filter
                        (\pictureAndItsDescription ->
                            File.name pictureAndItsDescription.picture /= picturesName
                        )
                        model.pictures
              }
            , Cmd.none
            )

        PictureDragStarted pictureName ->
            ( { model | pictureBeingDragged = Just pictureName }, Cmd.none )

        PictureDraggedOver pictureName ->
            ( { model | pictureBeingDraggedOver = Just pictureName }, Cmd.none )

        PictureDragEnded ->
            ( { model | pictureBeingDragged = Nothing, pictureBeingDraggedOver = Nothing }, Cmd.none )

        PictureDroppedOn p ->
            let
                --  This is where we "move" the picture being dragged to the position of the picture it was dropped on
                --  Remove the picture being dragged from the list
                --  Break the list into two parts: the part before the picture being dropped on and the part after
                --  Join the first part, the picture being dragged, and the second part together
                updatedPictures =
                    case model.pictureBeingDragged of
                        Just pictureBeingDragged ->
                            model.pictures
                                |> List.filter (\pictureAndItsDescription -> pictureAndItsDescription /= pictureBeingDragged)
                                |> List.sortBy .position
                                |> List.partition (\currentPicture -> currentPicture.position < p.position)
                                |> (\( before, after ) -> List.concat [ before, [ pictureBeingDragged ], after ])
                                |> List.indexedMap (\index currentPicture -> { currentPicture | position = index })

                        Nothing ->
                            model.pictures
            in
            ( { model | pictures = updatedPictures, pictureBeingDragged = Nothing }, Cmd.none )

        -- Download Documents
        DownloadDocumentsClicked ->
            ( model, renderThePDF <| getEncodedDocumentDefinition model.pictures )

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
                        viewDropZone

                    _ ->
                        viewPicturesList model
                ]

            -- Download Documents
            , div
                [ Attr.class "flex justify-end p-4" ]
                [ button
                    [ onClick DownloadDocumentsClicked
                    , buttonStyle
                    ]
                    [ text "Download Documents" ]
                ]
            ]
        ]


viewPicturesList : Model -> Html Msg
viewPicturesList model =
    div
        [ Attr.class "p-8 border border-grey-300 rounded flex gap-8 flex-wrap mx-auto"
        , onFilesDrop PicturesDropped
        , onDragOver NoOp
        ]
        (List.map (viewPicture model) model.pictures)


viewDropZone : Html Msg
viewDropZone =
    div
        [ Attr.class "flex flex-col gap-8 items-center justify-center p-8 w-full border-2 border-gray-100 hover:border-gray-200 hover:bg-gray-50 rounded"
        , onFilesDrop PicturesDropped
        , onDragOver NoOp
        ]
        [ div [ Attr.class "text-gray-300", disabled True ] [ text "Drag and drop pictures here" ]
        , button
            [ onClick AddPicturesClicked
            , buttonStyle
            ]
            [ text "Add pictures" ]
        ]


viewPicture : Model -> Picture -> Html Msg
viewPicture model picture =
    div
        [ Attr.class "group flex flex-col gap-2"
        , Attr.class
            (case model.pictureBeingDragged of
                Just p ->
                    if p == picture then
                        "opacity-50"

                    else
                        "opacity-100"

                Nothing ->
                    "opacity-100"
            )
        , Attr.class
            (case model.pictureBeingDraggedOver of
                Just p ->
                    if p == picture then
                        "border-dashed border-2 border-gray-200"

                    else
                        ""

                Nothing ->
                    ""
            )
        ]
        [ div
            [ Attr.class "relative bg-no-repeat bg-contain bg-center h-64 w-full"
            , Attr.style "background-image" ("url('" ++ picture.contentInURLFormat ++ "')")
            , onDragStart (PictureDragStarted picture)
            , onDragOver (PictureDraggedOver picture)
            , onDrop (PictureDroppedOn picture)
            , onDragEnd PictureDragEnded
            ]
            [ div
                [ Attr.class "invisible group-hover:visible absolute right-2 top-2 rounded-full bg-gray-100 border border-gray-200 p-2 hover:bg-gray-200"
                , onClick (DeletePictureClicked (File.name picture.picture))
                ]
                [ text "x" ]
            ]
        , input
            [ type_ "text"
            , placeholder "Description"
            , value picture.description
            , onInput (PicturesDescriptionInputted (File.name picture.picture))
            , Attr.class "group-focus:border group-focus:border-gray-300 rounded placeholder:text-gray-300"
            ]
            []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        []



-- EVENTS


onDragStart : Msg -> Html.Attribute Msg
onDragStart msg =
    preventDefaultOn "dragstart" (Decode.succeed ( msg, True ))


onDragOver : Msg -> Html.Attribute Msg
onDragOver msg =
    preventDefaultOn "dragover" (Decode.succeed ( msg, True ))


onDragEnd : Msg -> Html.Attribute Msg
onDragEnd msg =
    preventDefaultOn "dragend" (Decode.succeed ( msg, True ))


onDrop : Msg -> Html.Attribute Msg
onDrop msg =
    preventDefaultOn "drop" (Decode.succeed ( msg, True ))


onFilesDrop : (List File -> Msg) -> Html.Attribute Msg
onFilesDrop msg =
    preventDefaultOn "drop" (Decode.map2 Tuple.pair (Decode.map msg filesDecoder) (Decode.succeed True))



-- DECODERS


filesDecoder : Decode.Decoder (List File)
filesDecoder =
    Decode.at [ "dataTransfer", "files" ] (Decode.list File.decoder)



-- OTHER HELPER FUNCTIONS


buttonStyle : Html.Attribute msg
buttonStyle =
    Attr.class "bg-gray-900 border border-gray-900 rounded px-4 py-2 uppercase text-gray-200 hover:text-gray-100 text-sm font-semibold hover:bg-black"


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


makePictures : Int -> List File -> List Picture
makePictures lastPosition files =
    List.indexedMap
        (\index file ->
            let
                newPosition =
                    lastPosition + index + 1
            in
            { position = newPosition
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
            Task.attempt (GotPictureInUrlFormat (File.name p.picture)) (File.toUrl p.picture)
        )
        ps



-- PDF Document Description helpers


getEncodedDocumentDefinition : List Picture -> Encode.Value
getEncodedDocumentDefinition ps =
    -- This is where we combine the TitlePage and the PicturePages and put it together into an encoded object called "content" and send that out
    Encode.object
        [ ( "content", Encode.list identity (List.concat [ getEncodedTitlePage, getEncodedPicturePages ps ]) )
        , ( "pageOrientation", Encode.string "landscape" )
        , ( "pageSize", Encode.string "A4" )
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
        , ( "width", Encode.string "840" )
        , ( "fit", Encode.list identity [ Encode.int 842, Encode.int 500 ] ) -- The height has been reduced to accommodate the description
        , ( "alignment", Encode.string "center" )
        ]


getEncodedPictureDescription : String -> Encode.Value
getEncodedPictureDescription description =
    Encode.object
        [ ( "text", Encode.string description )
        , ( "fontSize", Encode.int 10 )
        , ( "pageBreak", Encode.string "after" )
        ]
