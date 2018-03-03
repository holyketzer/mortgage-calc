module Charts.StackedBarChart exposing (render)

import Array
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Color.Convert exposing (colorToCssRgb)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Shape as Shape exposing (StackConfig, StackResult)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Color exposing (Color)
import List.Extra as List


render : List(String, List Float) -> { width: Int, height: Int } -> Svg msg
render data windowSize =
    view (Shape.stack (config data)) { width = toFloat windowSize.width, height = toFloat windowSize.height/2 }

type alias Year = Int

padding : { bottom : number, left : number1, right : number2, top : number3 }
padding =
  {
    top = 30,
    left = 60,
    right = 30,
    bottom = 60
  }

config : List(String, List Float) -> StackConfig String
config data =
  {
    data = data,
    offset = Shape.stackOffsetNone,
    order = (\l -> l)
  }

colors : Int -> List String
colors size =
  ["#2d9f2c", "#ff7f0e", "#2076b4"]

column : BandScale Year -> (Year, List(Float, Float)) -> Svg msg
column xScale (year, values) =
  let
    block color ( upperY, lowerY ) =
      rect
        [ x <| toString <| Scale.convert xScale year
        , y <| toString <| lowerY
        , width <| toString <| Scale.bandwidth xScale
        , height <| toString <| (abs <| upperY - lowerY)
        , fill color
        ]
        []
  in
    g [ class "column" ] (List.map2 block (colors (List.length values)) values)

view : StackResult String -> { width: Float, height: Float } -> Svg msg
view { values, labels, extent } windowSize =
  let
    -- transpose back to get the values per year
    yearValues =
      List.transpose values

    years = List.range 1 (List.length yearValues)

    xScale : BandScale Year
    xScale =
      Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } years ( 0, windowSize.width - (padding.top + padding.bottom) )

    (minY, maxY) = extent

    fixedExtent =
      if minY == maxY then
        (minY, minY + 1)
      else
        (minY, maxY)

    yScale : ContinuousScale
    yScale =
      Scale.linear fixedExtent ( windowSize.height - (padding.left + padding.right), 0 )
        |> flip Scale.nice 4

    axisOptions =
      Axis.defaultOptions

    xAxis : Svg msg
    xAxis =
      Axis.axis { axisOptions | orientation = Axis.Bottom, tickCount = 10 } (Scale.toRenderable xScale)

    yAxis : Svg msg
    yAxis =
      Axis.axis { axisOptions | orientation = Axis.Left } yScale

    scaledValues =
      List.map (List.map (\( y1, y2 ) -> ( Scale.convert yScale y1, Scale.convert yScale y2 ))) yearValues
  in
    svg [ width (toString windowSize.width ++ "px"), height (toString windowSize.height ++ "px") ] [
      g [ translate (padding.left - 1) (windowSize.height - padding.bottom) ] [ xAxis ],
      g [ translate (padding.left - 1) padding.top ] [ yAxis ],
      g [ translate padding.left padding.top, class "series" ] <|
            List.map (column xScale) (List.map2 (,) years scaledValues)
    ]

translate : number -> number -> Svg.Attribute msg
translate x y =
  transform ("translate(" ++ toString x ++ ", " ++ toString y ++ ")")
