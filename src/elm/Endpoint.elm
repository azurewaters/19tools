module Endpoint exposing (..)

import Html exposing (Html, text)
import Html.Attributes exposing (href)
import Url.Builder exposing (QueryParameter)



-- TYPES


type Endpoint
    = Endpoint String



-- TYPE FUNCTIONS


unwrap : Endpoint -> String
unwrap (Endpoint s) =
    s


url : List String -> List QueryParameter -> Endpoint
url paths queryParameters =
    Url.Builder.crossOrigin "" paths queryParameters
        |> Endpoint


a : Endpoint -> String -> Html msg
a endpoint linkText =
    Html.a [ href <| unwrap endpoint ] [ text linkText ]



-- ENDPOINTS
-- GENERAL


home : Endpoint
home =
    url [] []



-- CANADIAN IMMIGRATION ENDPOINTS


proofOfRelationshipHome : Endpoint
proofOfRelationshipHome =
    url [ "proofofrelationship" ] []


proofOfContact : Endpoint
proofOfContact =
    url [ "proofofcontact" ] []


index : Endpoint
index =
    url [ "index" ] []
