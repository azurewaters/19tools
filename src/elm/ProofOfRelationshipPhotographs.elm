port module ProofOfRelationshipPhotographs exposing (Model, Msg, init, initialModel, subscriptions, update, view)

import Accessibility exposing (h2)
import Bitwise exposing (or)
import Bytes
import File exposing (File)
import File.Download
import File.Select
import Html exposing (Html, button, div, hr, img, input, label, p, section, text, textarea)
import Html.Attributes as Attr exposing (class, classList, disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import Json.Decode as Decode
import Json.Encode as Encode
import ProofOfContact exposing (Model)
import Task



-- MODEL


type alias Model =
    { applicantsLastName : String
    , applicantsOtherNames : String
    , sponsorsName : String
    , pictures : List Picture
    , pictureBeingDragged : Maybe Picture
    , pictureBeingDraggedOver : Maybe Picture
    , debugMessage : String
    , showErrors : Bool
    }


type alias Picture =
    { position : Int
    , name : String
    , contents : String
    , description : String
    , width : Int
    , height : Int
    }


type alias ResizedPicture =
    { name : String
    , contents : String
    , width : Int
    , height : Int
    }


initialModel : Model
initialModel =
    { applicantsLastName = ""
    , applicantsOtherNames = ""
    , sponsorsName = ""
    , pictures = []
    , pictureBeingDragged = Nothing
    , pictureBeingDraggedOver = Nothing
    , debugMessage = ""
    , showErrors = False
    }



-- INIT


init : Model -> ( Model, Cmd Msg )
init model =
    ( model
    , Cmd.none
    )



-- MESSAGE


type Msg
    = -- Collect Details
      ApplicantsLastNameInputted String
    | ApplicantsOtherNamesInputted String
    | SponsorsNameInputted String
      -- Upload Pictures
    | PicturesDropped (List File)
    | PicturesSelected File (List File)
    | AddPicturesClicked
    | GotPictureInUrlFormat String (Result String String)
    | GotResizedPicture ResizedPicture
    | PicturesDescriptionInputted String String
    | DeletePictureClicked String
    | PictureDragStarted Picture
    | PictureDraggedOver Picture
    | PictureDragEnded
    | PictureDroppedOn Picture
      -- Download Documents
    | DownloadDocumentsClicked
    | GotTheProofOfRelationship File
    | GotTheProofOfRelationshipsContents Bytes.Bytes
      -- Other
    | Log String
    | NoOp



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Home
        ApplicantsLastNameInputted name ->
            ( { model | applicantsLastName = name }, Cmd.none )

        ApplicantsOtherNamesInputted name ->
            ( { model | applicantsOtherNames = name }, Cmd.none )

        SponsorsNameInputted name ->
            ( { model | sponsorsName = name }, Cmd.none )

        PicturesDropped files ->
            case model.pictureBeingDragged of
                Nothing ->
                    let
                        -- Filter out the files that are already uploaded
                        -- Make a PictureAndItsDescription for each file with its id set to the next id
                        newPictures =
                            files
                                |> getOnlyNewFiles model.pictures
                                |> makePictures (List.length model.pictures)

                        newPicturesFileNames =
                            newPictures
                                |> List.map .name

                        newFiles =
                            files
                                |> List.filter
                                    (\f ->
                                        List.member (File.name f) newPicturesFileNames
                                    )
                    in
                    ( { model
                        | pictures = model.pictures ++ newPictures
                      }
                    , Cmd.batch (getPicturesInUrlFormatCmds newFiles)
                    )

                Just _ ->
                    ( model, Cmd.none )

        PicturesSelected file files ->
            let
                newPictures =
                    (file :: files)
                        |> getOnlyNewFiles model.pictures
                        |> makePictures (List.length model.pictures)

                newPicturesFileNames =
                    newPictures
                        |> List.map .name

                newFiles =
                    (file :: files)
                        |> List.filter
                            (\f ->
                                List.member (File.name f) newPicturesFileNames
                            )
            in
            ( { model | pictures = model.pictures ++ newPictures }
            , Cmd.batch (getPicturesInUrlFormatCmds newFiles)
            )

        AddPicturesClicked ->
            ( model, File.Select.files [ "image/png", "image/jpg", "image/jpeg" ] PicturesSelected )

        GotPictureInUrlFormat picturesName result ->
            case result of
                Ok pictureInUrlFormat ->
                    ( model
                    , Cmd.batch
                        [ resizePicture <|
                            Encode.object
                                [ ( "name", Encode.string picturesName )
                                , ( "contents", Encode.string pictureInUrlFormat )
                                ]
                        ]
                    )

                Err _ ->
                    ( model, Cmd.none )

        GotResizedPicture resizedPicture ->
            let
                updatedPictures : List Picture
                updatedPictures =
                    List.map
                        (\picture ->
                            if picture.name == resizedPicture.name then
                                { picture
                                    | contents = resizedPicture.contents
                                    , width = resizedPicture.width
                                    , height = resizedPicture.height
                                }

                            else
                                picture
                        )
                        model.pictures
            in
            ( { model
                | pictures = updatedPictures
                , debugMessage = "Got the resized picture"
              }
            , Cmd.none
            )

        PicturesDescriptionInputted name description ->
            ( { model
                | pictures =
                    List.map
                        (\picture ->
                            if picture.name == name then
                                { picture | description = description }

                            else
                                picture
                        )
                        model.pictures
              }
            , Cmd.none
            )

        DeletePictureClicked picturesName ->
            ( { model
                | pictures =
                    List.filter
                        (\picture ->
                            picture.name /= picturesName
                        )
                        model.pictures
              }
            , Cmd.none
            )

        PictureDragStarted picture ->
            ( { model | pictureBeingDragged = Just picture, debugMessage = "Drag started" }, Cmd.none )

        PictureDraggedOver picture ->
            ( { model | pictureBeingDraggedOver = Just picture }, Cmd.none )

        PictureDragEnded ->
            ( { model
                | pictureBeingDragged = Nothing
                , pictureBeingDraggedOver = Nothing
                , debugMessage = "Drag ended"
              }
            , Cmd.none
            )

        PictureDroppedOn picture ->
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
                                |> List.partition (\currentPicture -> currentPicture.position < picture.position)
                                |> (\( before, after ) -> List.concat [ before, [ pictureBeingDragged ], after ])
                                |> List.indexedMap (\index currentPicture -> { currentPicture | position = index })

                        Nothing ->
                            model.pictures
            in
            ( { model
                | pictures = updatedPictures
                , pictureBeingDragged = Nothing
                , debugMessage = "Dropped"
              }
            , Cmd.none
            )

        DownloadDocumentsClicked ->
            --  First, we check to see if all the details that we wanted were typed in
            --  If not, add the "required" class to the missing fields
            --  All the fields are required fields
            if model.applicantsLastName == "" || model.applicantsOtherNames == "" || model.sponsorsName == "" || List.length model.pictures == 0 then
                ( { model | showErrors = True }, Cmd.none )

            else
                -- All required information has been filled in. Go ahead and make the PDF.
                -- This is where we take the Picture records and encode them into a JSON string and update the value in the model with it.
                ( { model | showErrors = False }
                , renderTheProofOfRelationship <|
                    Encode.object
                        [ ( "sponsorsName", Encode.string model.sponsorsName )
                        , ( "pictures"
                          , model.pictures
                                |> List.map
                                    (\picture ->
                                        Encode.object
                                            [ ( "position", Encode.int picture.position )
                                            , ( "name", Encode.string picture.name )
                                            , ( "contents", Encode.string picture.contents )
                                            , ( "description", Encode.string picture.description )
                                            , ( "width", Encode.int picture.width )
                                            , ( "height", Encode.int picture.height )
                                            ]
                                    )
                                |> Encode.list identity
                          )
                        ]
                )

        GotTheProofOfRelationship file ->
            -- Read the PDF as bytes and then download it
            ( { model | debugMessage = "Got the proof of proof of relationship" }, Task.perform GotTheProofOfRelationshipsContents (File.toBytes file) )

        GotTheProofOfRelationshipsContents bytes ->
            let
                -- Remove all special characters from the name to make it a valid file name
                lastNameForFileName =
                    String.filter (\c -> Char.isAlphaNum c || c == ' ') model.applicantsLastName

                otherNamesForFileName =
                    String.filter (\c -> Char.isAlphaNum c || c == ' ') model.applicantsOtherNames

                documentName =
                    lastNameForFileName
                        ++ " - "
                        ++ otherNamesForFileName
                        ++ " - Proof of Relationship - Photographs"
                        ++ ".pdf"
            in
            ( { model | debugMessage = model.debugMessage ++ " Got the contents" }, File.Download.bytes documentName "application/pdf" bytes )

        -- Other
        Log message ->
            ( { model | debugMessage = message }, Cmd.none )

        NoOp ->
            ( { model | debugMessage = "Came to NoOp from somewhere" }, Cmd.none )



-- PORTS


port resizePicture : Encode.Value -> Cmd msg


port renderTheProofOfRelationship : Encode.Value -> Cmd msg



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "container max-w-2xl mx-auto flex flex-col gap-16"
        ]
        [ -- About the tool
          section
            [ class "flex flex-col gap-2"
            ]
            [ h2
                [ class "mb-10" ]
                [ text "about" ]
            , p
                [ class "text-slate-600" ]
                [ text "The Immigration, Refugees and Citizenship of Canada (IRCC) mandates proof of relationship for specific immigration applications. Our tool helps organize photos and their descriptions into a formatted document, vhile compressing large images." ]
            ]

        -- The tool
        , section
            [ class "coloured flex flex-col gap-8" ]
            [ -- Collect Details
              h2 [] [ text "details" ]
            , div [ class "flex flex-col gap-2" ]
                [ label [] [ text "applicant's last name" ]
                , input [ onInput ApplicantsLastNameInputted, type_ "text", placeholder "", value model.applicantsLastName, classList [ ( "error", model.applicantsLastName == "" && model.showErrors ) ] ] []
                , div [ class "textExplainer -mt-1" ] [ text "Use the applicant's last name as it appears on a passport." ]
                ]
            , div [ class "flex flex-col gap-2" ]
                [ label [] [ text "applicant's other names" ]
                , input [ onInput ApplicantsOtherNamesInputted, type_ "text", placeholder "", value model.applicantsOtherNames, classList [ ( "error", model.applicantsOtherNames == "" && model.showErrors ) ] ] []
                , div [ class "textExplainer -mt-1" ] [ text "Type in the applicant's first, middle and other names as they appear on a passport." ]
                ]
            , div [ class "flex flex-col gap-2" ]
                [ label [] [ text "sponsor's name" ]
                , input [ onInput SponsorsNameInputted, type_ "text", placeholder "", value model.sponsorsName, classList [ ( "error", model.sponsorsName == "" && model.showErrors ) ] ] []
                , div [ class "textExplainer -mt-1" ] [ text "Type in the sponsor's full name here." ]
                ]

            -- Upload Pictures
            , div
                [ class "flex flex-col gap-2" ]
                [ label [] [ text "Pictures" ]
                , viewPicturesList model
                , button
                    [ onClick AddPicturesClicked
                    , class "w-max self-end"
                    ]
                    [ text "Add pictures" ]
                ]

            -- Download Documents
            , div
                [ class "flex justify-end" ]
                [ button
                    [ onClick DownloadDocumentsClicked
                    ]
                    [ text "Download Documents" ]
                ]
            ]
        ]


viewPicturesList : Model -> Html Msg
viewPicturesList model =
    div
        [ class "mb-2 p-8 border border-gray-800 rounded-sm bg-white flex flex-row flex-wrap gap-10 justify-center"
        , classList [ ( "error", List.length model.pictures == 0 && model.showErrors ) ]
        , onFilesDrop PicturesDropped
        , onDragOver NoOp
        ]
        (case model.pictures of
            [] ->
                [ div
                    [ class "text-gray-400", disabled True ]
                    [ text "drop pictures here" ]
                ]

            _ ->
                List.map (viewPicture model) model.pictures
        )


viewPicture : Model -> Picture -> Html Msg
viewPicture model picture =
    div
        [ class "relative group border border-gray-400 p-8 rounded w-full flex flex-col gap-8"

        -- , class
        --     (case model.pictureBeingDragged of
        --         Just p ->
        --             if p == picture then
        --                 "opacity-50"
        --             else
        --                 "opacity-100"
        --         Nothing ->
        --             "opacity-100"
        --     )
        -- , class
        --     (case model.pictureBeingDraggedOver of
        --         Just p ->
        --             if p == picture then
        --                 "border-dashed border-2 border-gray-200"
        --             else
        --                 ""
        --         Nothing ->
        --             ""
        --     )
        , Attr.draggable "true"
        , onDragStart (PictureDragStarted picture)
        , onDrop (PictureDroppedOn picture)
        , onDragEnd PictureDragEnded
        ]
        [ div
            [ class "bg-no-repeat bg-contain bg-top h-64"
            , Attr.style "background-image" ("url('" ++ picture.contents ++ "')")
            ]
            []
        , textarea
            [ placeholder "Description"
            , value picture.description
            , onInput (PicturesDescriptionInputted picture.name)
            , class "text-sm border border-gray-400 rounded placeholder:text-gray-300"
            ]
            []
        , div
            [ class "invisible group-hover:visible absolute right-4 top-4 w-8 h-8 p-2 rounded-full bg-gray-100 border border-gray-200 hover:bg-gray-200"
            , onClick (DeletePictureClicked picture.name)
            ]
            [ img [ src "assets/x.svg", class "stroke-black" ] [] ]
        ]



-- SUBSCRIPTIONS


port gotResizedPicture : (Decode.Value -> msg) -> Sub msg


port gotTheProofOfRelationship : (Decode.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ gotResizedPicture decodeResizedPicture
        , gotTheProofOfRelationship decodeTheProofOfRelationship
        ]



-- EVENTS


onDragStart : Msg -> Html.Attribute Msg
onDragStart msg =
    Html.Events.on "dragstart" (Decode.succeed msg)


onDragOver : Msg -> Html.Attribute Msg
onDragOver msg =
    preventDefaultOn "dragover" (Decode.succeed ( msg, True ))


onDragEnd : Msg -> Html.Attribute Msg
onDragEnd msg =
    Html.Events.on "dragend" (Decode.succeed msg)


onDrop : Msg -> Html.Attribute Msg
onDrop msg =
    preventDefaultOn "drop" (Decode.succeed ( msg, True ))


onFilesDrop : (List File -> Msg) -> Html.Attribute Msg
onFilesDrop msg =
    preventDefaultOn "drop" (Decode.map2 Tuple.pair (Decode.map msg filesDecoder) (Decode.succeed True))



-- DECODERS


decodeResizedPicture : Decode.Value -> Msg
decodeResizedPicture value =
    case Decode.decodeValue resizedPictureDecoder value of
        Ok result ->
            GotResizedPicture result

        Err _ ->
            Log "Failed to decode the resized picture: "


decodeTheProofOfRelationship : Decode.Value -> Msg
decodeTheProofOfRelationship value =
    Decode.decodeValue File.decoder value
        |> Result.map GotTheProofOfRelationship
        |> Result.withDefault (Log "Failed to decode the proof of relationship")


filesDecoder : Decode.Decoder (List File)
filesDecoder =
    Decode.at [ "dataTransfer", "files" ] (Decode.list File.decoder)


resizedPictureDecoder : Decode.Decoder ResizedPicture
resizedPictureDecoder =
    Decode.map4
        ResizedPicture
        (Decode.field "name" Decode.string)
        (Decode.field "contents" Decode.string)
        (Decode.field "width" <| Decode.oneOf [ Decode.int, Decode.float |> Decode.map Basics.floor ])
        (Decode.field "height" <| Decode.oneOf [ Decode.int, Decode.float |> Decode.map Basics.floor ])



-- OTHER HELPER FUNCTIONS


getOnlyNewFiles : List Picture -> List File -> List File
getOnlyNewFiles pictures files =
    List.filter
        (\file ->
            List.all
                (\p ->
                    p.name /= File.name file
                )
                pictures
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
            , name = File.name file
            , contents = ""
            , description = ""
            , width = 0
            , height = 0
            }
        )
        files


getPicturesInUrlFormatCmds : List File -> List (Cmd Msg)
getPicturesInUrlFormatCmds files =
    List.map
        (\f ->
            Task.attempt (GotPictureInUrlFormat (File.name f)) (File.toUrl f)
        )
        files
