module Home exposing (..)

import Accessibility exposing (h3)
import Endpoint exposing (..)
import Html exposing (Html, button, div, h2, hr, img, p, section, text)
import Html.Attributes as Attr exposing (class, href, src)



-- MODEL


type alias Model =
    { menuOptions : List MenuOption }


type alias MenuOption =
    { title : String
    , description : String
    , endpoint : Endpoint
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model []
    , Cmd.none
    )



-- UPDATE


type Msg
    = MenuOptionProofOfRelationshipClicked
    | MenuOptionProofOfContactClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MenuOptionProofOfRelationshipClicked ->
            ( model, Cmd.none )

        MenuOptionProofOfContactClicked ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view _ =
    div
        [ class "container max-w-4xl mx-auto flex flex-col gap-16 mt-8"
        ]
        [ section
            [ class "flex flex-row flex-wrap gap-8 items-center" ]
            [ div
                [ class "flex-1 flex flex-col gap-8" ]
                [ h2 [] [ text "about" ]
                , p [ class "text-slate-600" ] [ text "Amet deserunt deserunt nisi pariatur dolore ex incididunt commodo amet nisi veniam eiusmod. Anim minim eiusmod eiusmod ea tempor reprehenderit velit sit cupidatat sunt. Adipisicing sit quis nisi elit adipisicing ut anim non reprehenderit culpa mollit dolore occaecat. Enim magna esse ea est Lorem eiusmod labore veniam. Dolor anim quis sunt velit voluptate excepteur nostrud culpa elit. Sunt consectetur ullamco aute ad. Lorem consequat ipsum mollit ullamco nulla esse ipsum consequat sunt quis quis elit tempor." ]
                ]
            , div [ class "flex-1" ] [ img [ src "assets/hompage-illustration.svg" ] [] ]
            ]
        , menuOptionGroup
            "immigration tools"
            [ MenuOption "Proof of relationship - Photographs" "Produce a document that helps prove your relationship to your sponsor." Endpoint.proofOfRelationshipHome
            , MenuOption "Proof of contact" "Produce a document that helps prove you have had contact with your sponsor." Endpoint.proofOfContact
            , MenuOption "Index" "Produce a document that puts a collection of documents together." Endpoint.index
            ]
        ]


menuOptionGroup : String -> List MenuOption -> Html Msg
menuOptionGroup title menuOptionDetails =
    section
        [ class "coloured flex flex-col gap-8"
        ]
        [ h2 [] [ text title ]
        , div [ class "grid grid-cols-1 sm:grid-cols-2 gap-8" ] (List.map menuOption menuOptionDetails)
        ]


menuOption : MenuOption -> Html Msg
menuOption menuOptionDetails =
    Html.a
        [ class "bg-white border border-slate-600 flex flex-col items-start p-8 gap-4 rounded max-w-xs"
        , href (Endpoint.unwrap menuOptionDetails.endpoint)
        ]
        [ h3 [] [ text menuOptionDetails.title ]
        , div [ class "flex-1" ] [ text menuOptionDetails.description ]
        , div [ class "w-full flex flex-row justify-end" ] [ button [ class "small" ] [ text "go" ] ]
        ]
