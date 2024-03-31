module Dimensions exposing (Measurements, a4, pixelsToPointsFactor, pointsToPixelsFactor)

-- This module contains functions that help convert from pixels to points and to scale dimensions


type alias Width =
    Float


type alias Height =
    Float


type Measurements
    = Points Width Height
    | Pixels Width Height


a4 =
    { width = 595, height = 842 }


pointsToPixelsFactor : Float
pointsToPixelsFactor =
    1.3333333333


pixelsToPointsFactor : Float
pixelsToPointsFactor =
    -- 1 pixel = 0.75 points
    0.75


convertToPoints : Measurements -> Measurements
convertToPoints measurements =
    case measurements of
        Points _ _ ->
            measurements

        Pixels width height ->
            Points (width * pixelsToPointsFactor) (height * pixelsToPointsFactor)


convertToPixels : Measurements -> Measurements
convertToPixels measurements =
    case measurements of
        Pixels _ _ ->
            measurements

        Points width height ->
            Pixels (width * pointsToPixelsFactor) (height * pointsToPixelsFactor)
