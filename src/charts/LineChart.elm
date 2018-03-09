module Charts.LineChart exposing (render)

import Date exposing (Date)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.List as List
import Visualization.Scale as Scale exposing (ContinuousScale, ContinuousTimeScale)
import Visualization.Shape as Shape

type alias ChartSize = {
  w: Float,
  h: Float
}

type alias Range = (Float, Float)

padding : Float
padding =
  60

xScale : Float -> Range -> ContinuousScale
xScale w range =
  Scale.linear range (0, w - 2 * padding)

yScale : Float -> Range -> ContinuousScale
yScale h range =
  Scale.linear range (h - 2 * padding, 0)

getRange accessor model =
  let
    min =
      case List.minimum <| List.map accessor model of
        Just value -> value
        Nothing -> 0
    max =
      case List.maximum <| List.map accessor model of
        Just value -> value
        Nothing -> 1
  in
    (min, max)

xAxis : List(Float, Float) -> Float -> Svg msg
xAxis model w =
  Axis.axis { defaultOptions | orientation = Axis.Bottom, tickCount = 20 } (xScale w (getRange Tuple.first model))


yAxis : List(Float, Float) -> Float -> Svg msg
yAxis model h =
  Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 10 } (yScale h (getRange Tuple.second model))

rightYAxis : List(Float, Float) -> Float -> Svg msg
rightYAxis model h =
  Axis.axis { defaultOptions | orientation = Axis.Right, tickCount = 10 } (yScale h (getRange Tuple.second model))

transformToLineData : List(Float, Float) -> ChartSize -> (Float, Float) -> Maybe(Float, Float)
transformToLineData model { w, h } (x, y)  =
  Just(Scale.convert (xScale w (getRange Tuple.first model)) x, Scale.convert (yScale h (getRange Tuple.second model)) y)

line : List(Float, Float) -> ChartSize -> Attribute msg
line model size =
  List.map (transformToLineData model size) model
    |> Shape.line Shape.linearCurve
    |> d


area : List(Float, Float) -> ChartSize -> Attribute msg
area model size =
  List.map (transformToLineData model size) model
    |> Shape.line Shape.linearCurve
    |> d


view : List (Float, Float) -> List (Float, Float) -> ChartSize -> (String, String) -> Svg msg
view lineModel areaModel size labels =
  let
    hintData = List.map2 (,) lineModel areaModel

    showHint ((x1, y1), (_, y2)) =
      let
        lineStep = 17
        maxHintWidth = 150
        scale = xScale size.w (getRange Tuple.first lineModel)
        xPos = Scale.convert scale x1
        xHintPos = Basics.min xPos ((Tuple.second <| Scale.range scale) - maxHintWidth)

        hintLine index label value color =
          g [] [
            rect [x <| toString xHintPos, y <| toString <| index * lineStep - 12, width "14", height "14", fill color, stroke "black"] [],
            text_ [x <| toString (xHintPos + lineStep), y <| toString <| index * lineStep] [text <| label ++ ": " ++ toString value]
          ]
      in
        g [class "hintHost"] [
          rect [x <| toString xPos, y "0", width "20", height <| toString (size.h - 2 * padding), fillOpacity "0.0"] [],
          g [class "hint"] [
            rect [x <| toString xPos, y "0", width "1", height <| toString (size.h - 2 * padding), fillOpacity "0.3"] [],
            hintLine -1 (Tuple.first labels) y1 "green",
            hintLine 0 (Tuple.second labels) y2 "red"
          ]
        ]
  in
    svg [width (toString size.w ++ "px"), height (toString size.h ++ "px")] [
      Svg.style [] [
        text """
          .hintHost .hint { display: none; }
          .hintHost:hover .hint { display: inline; }
        """
      ],
      g [transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (size.h - padding) ++ ")")]
        [xAxis lineModel size.w],

      g [transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")")]
        [yAxis lineModel size.h],

      g [transform ("translate(" ++ toString (size.w - padding - 1) ++ ", " ++ toString padding ++ ")")]
        [rightYAxis areaModel size.h],

      g [transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")"), class "series"] [
        Svg.path [ line lineModel size, stroke "red", strokeWidth "1px", fill "none" ] [],
        Svg.path [ area areaModel size, stroke "green", strokeWidth "1px", fill "none" ] [],
        g [] (List.map showHint hintData)
      ]
    ]

-- From here onwards this is simply example boilerplate.
-- In a real app you would load the data from a server and parse it, perhaps in
-- a separate module.

render lineModel areaModel windowSize labels =
  view lineModel areaModel { w = toFloat windowSize.width, h = toFloat windowSize.height/2 } labels
