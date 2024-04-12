port module ProofOfContact exposing (..)

import File exposing (File)
import File.Select
import Html exposing (Html, button, div, h2, input, label, p, section, text, textarea)
import Html.Attributes exposing (class, disabled, type_)
import Html.Events as Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Task



{--This is an Elm/Typescript app that will take pictures from the user and produce a PDF of them.

In the Elm part, we will collect these details from the user:

| Field                                                     | Required? | Default Value                                                                               |
| --------------------------------------------------------- | --------- | ------------------------------------------------------------------------------------------- |
| The title of the document.                                | Yes       | Proof of Contact                                                                            |
| The description of the document.                          | Yes       | On the following pages is proof that my sponsor and I have stayed in touch with each other. |
| The name that will be given to the PDF file.              | Yes       | ProofOfContact.pdf                                                                          |
| The files that the user wants to put together in the PDF. | Yes       |                                                                                             |

We have to show the user an error if he has missed filling in any of the required details. If he hasn't, then we inform the user that those details are required and should not proceed with producing the PDF.

If all the details are present, then all of it is sent to TypeScript when the user presses the "download" button, through a port. The TypeScript makes the PDF and sends a File object back to Elm. This file is read into bytes, and then downloaded to the user's system with the file name that he gave.

Please give me the Elm code for the model that the above app will need. Make sure that the names of variables, fields and everything else are descriptive, even if they are long.
--}
-- MODEL


type alias Model =
    { title : String
    , description : String
    , filename : String
    , filesAndTheirContents : List FileAndContents
    , allRequiredDetailsProvided : Bool
    }


type alias FileAndContents =
    { name : String
    , contents : String
    , mimeType : String
    }


initialModel : Model
initialModel =
    { title = "Proof of Contact"
    , description = "On the following pages is proof that my sponsor and I have stayed in touch with each other."
    , filename = "ProofOfContact.pdf"
    , filesAndTheirContents = []
    , allRequiredDetailsProvided = False
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type alias FileName =
    String


type alias FileContents =
    String


type Msg
    = NoOp
    | TitleChanged String
    | DescriptionChanged String
    | FilenameChanged String
    | FilesDropped (List File)
    | DropzoneClicked
    | FilesSelected File (List File)
    | FileContentsRead FileName FileContents
    | RemoveFile FileName
    | DownloadClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TitleChanged newTitle ->
            ( { model | title = newTitle }, Cmd.none )

        DescriptionChanged newDescription ->
            ( { model | description = newDescription }, Cmd.none )

        FilenameChanged newFilename ->
            ( { model | filename = newFilename }, Cmd.none )

        FilesDropped files ->
            -- This is where we make sure that only files that haven't already been dropped in are added.
            --  The contents of the files are read and stored in the model.
            let
                existingFilesNames =
                    List.map .name model.filesAndTheirContents

                newFiles =
                    List.filter (\file -> not (List.member (File.name file) existingFilesNames)) files

                newFilesAndTheirContents =
                    List.map
                        (\file ->
                            { name = File.name file
                            , contents = ""
                            , mimeType = File.mime file
                            }
                        )
                        newFiles

                commandsToReadTheContentsOfNewFiles =
                    List.map
                        (\file ->
                            File.toUrl file
                                |> Task.perform (\contents -> FileContentsRead (File.name file) contents)
                        )
                        newFiles
            in
            ( { model | filesAndTheirContents = newFilesAndTheirContents }, Cmd.batch commandsToReadTheContentsOfNewFiles )

        FilesSelected file files ->
            -- This is where we make sure that only files that haven't already been selected are added.
            let
                existingFilesNames =
                    List.map .name model.filesAndTheirContents

                newFiles =
                    List.filter (\f -> not (List.member (File.name f) existingFilesNames)) (file :: files)
                        |> List.map (\f -> { name = File.name f, contents = "", mimeType = File.mime f })
            in
            ( { model | filesAndTheirContents = newFiles }, Cmd.none )

        DropzoneClicked ->
            ( model, File.Select.files [ "image/png", "image/jpg", "image/jpeg" ] FilesSelected )

        FileContentsRead filename contents ->
            let
                newFilesAndTheirContents =
                    List.map
                        (\fatc ->
                            if fatc.name == filename then
                                { fatc | contents = contents }

                            else
                                fatc
                        )
                        model.filesAndTheirContents
            in
            ( { model | filesAndTheirContents = newFilesAndTheirContents }, Cmd.none )

        RemoveFile filename ->
            ( { model | filesAndTheirContents = List.filter (\f -> f.name /= filename) model.filesAndTheirContents }, Cmd.none )

        DownloadClicked ->
            -- This is where we check if all the required details are present.
            let
                allRequiredDetailsProvided =
                    model.title
                        /= ""
                        && model.description
                        /= ""
                        && model.filename
                        /= ""
                        && List.length model.filesAndTheirContents
                        > 0

                command =
                    if allRequiredDetailsProvided then
                        -- This is where we send all the details to TypeScript.
                        --  First, we read the contents of the files using File.toUrl
                        --  Next, we encode each file's contents
                        --  Encode all the details that we need to send into a JSON object and send it to TypeScript.
                        Encode.object
                            [ ( "title", Encode.string model.title )
                            , ( "description", Encode.string model.description )
                            , ( "filename", Encode.string model.filename )
                            , ( "filesAndTheirContents"
                              , Encode.list
                                    (\f ->
                                        Encode.object
                                            [ ( "name", Encode.string f.name )
                                            , ( "contents", Encode.string f.contents )
                                            , ( "mimeType", Encode.string f.mimeType )
                                            ]
                                    )
                                    model.filesAndTheirContents
                              )
                            ]
                            |> makeThePDF

                    else
                        -- This is where we inform the user that all the required details are needed.
                        Cmd.none
            in
            ( { model | allRequiredDetailsProvided = allRequiredDetailsProvided }, command )


port makeThePDF : Decode.Value -> Cmd msg



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "container max-w-2xl mx-auto flex flex-col gap-16" ]
        [ -- About section
          section
            []
            [ h2
                [ class "mb-10" ]
                [ text "about" ]
            , p
                []
                [ text "Lorem ipsum dolor..." ]
            ]

        -- Details section
        --  This is where we collect all the details we need to produce the PDF.
        , section
            [ class "coloured" ]
            [ h2
                [ class "mb-10" ]
                [ text "details" ]
            , div
                [ class "flex flex-col gap-8" ]
                [ div
                    [ class "flex flex-col gap-2" ]
                    [ label [] [ text "title" ]
                    , input
                        [ type_ "text"
                        , onInput TitleChanged
                        ]
                        []
                    ]
                , div
                    [ class "flex flex-col gap-2" ]
                    [ label [] [ text "description" ]
                    , textarea
                        [ onInput DescriptionChanged ]
                        []
                    ]
                , div
                    [ class "flex flex-col gap-2" ]
                    [ label [] [ text "filename" ]
                    , input
                        [ type_ "text"
                        , onInput FilenameChanged
                        ]
                        []
                    ]
                , div
                    [ class "flex flex-col gap-2" ]
                    [ label [] [ text "files" ]
                    , dropzone
                    , listOfFiles model.filesAndTheirContents
                    ]
                ]
            ]
        , div
            [ onFilesDrop FilesDropped
            , onDragOver NoOp
            , class "w-full h-16 bg-white border border-slate-600"
            ]
            []
        ]



-- The place where files are dropped or selected by either dragging files onto the zone or the zone being clicked


dropzone : Html Msg
dropzone =
    div
        [ class "w-full p-8 flex justify-center border border-slate-600 border-dotted rounded bg-white text-slate-400"
        , onFilesDrop FilesDropped
        , onDragOver NoOp
        , onClick DropzoneClicked
        ]
        [ div [ disabled True ] [ text "Drop files here or click to select files" ] ]


listOfFiles : List FileAndContents -> Html Msg
listOfFiles files =
    div [ class "w-full" ] (List.map fileDetails files)


fileDetails : FileAndContents -> Html Msg
fileDetails file =
    div
        [ class "p-4 flex flex-row gap-2 items-center border border-slate-400 rounded bg-slate-200" ]
        [ div [ class "flex-1" ] [ text file.name ]
        , button [ onClick (RemoveFile file.name) ] [ text "x" ]
        ]



-- EVENTS


onDragStart : Msg -> Html.Attribute Msg
onDragStart msg =
    Events.on "dragstart" (Decode.succeed msg)


onDragOver : Msg -> Html.Attribute Msg
onDragOver msg =
    Events.preventDefaultOn "dragover" (Decode.succeed ( msg, True ))


onDragEnd : Msg -> Html.Attribute Msg
onDragEnd msg =
    Events.on "dragend" (Decode.succeed msg)



-- General purpose drop


onDrop : Msg -> Html.Attribute Msg
onDrop msg =
    Events.preventDefaultOn "drop" (Decode.succeed ( msg, True ))



-- Files drop


onFilesDrop : (List File -> Msg) -> Html.Attribute Msg
onFilesDrop msg =
    Events.preventDefaultOn "drop" (Decode.map2 Tuple.pair (Decode.map msg filesDecoder) (Decode.succeed True))



-- DECODERS


filesDecoder : Decode.Decoder (List File)
filesDecoder =
    Decode.list File.decoder
