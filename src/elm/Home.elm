module Home exposing (..)

import Html exposing (Html, button, div, h1, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)



-- MODEL


type alias Model =
    { menuOptions : List MenuOptionDetails }


type alias MenuOptionDetails =
    { title : String
    , description : String
    , msg : Msg
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
