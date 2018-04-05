module View exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Round

import Charts.LineChart
import Charts.StackedBarChart
import Events exposing (..)
import Model exposing (..)
import Shared exposing (roundMoney)

fieldType field =
  case field.kind of
    IntValue -> "number"
    _ -> "text"

inputField field msg label =
  div [] [
    text (label ++ ": "),
    input [ type_ (fieldType field), placeholder label, onInput msg, value field.raw] [],
    text (" " ++ field.error)
  ]

selectField field msg label options =
  div [] [
    text (label ++ ": "),
    select [ onInput msg ] (List.map (\x -> someToOption x field.value) options),
    text (" " ++ field.error)
  ]

someToOption (x, label) current =
  option [value (toString x), selected (x == current)] [text label]

mortgageForm model =
  div [] [
    inputField model.amount AmountChanged "Кредит",
    inputField model.period PeriodChanged "Период (мес.)",
    inputField model.interestRate InterestRateChanged "Процентная ставка по кредиту",
    inputField model.earlyPrincipal EarlyPrincipalChanged "Досрочное погашение (ежемесячно)",
    inputField model.depositInterest DepositInterestChanged "Процентная ставка по депозиту",
    selectField model.depositCapitalization DepositCapitalizationChanged "Капитализация депозита" [
      (Monthly, "Раз в месяц"),
      (Yearly, "Раз в год")
    ]
  ]

prettyPrice price =
  Round.round 2 price

renderPayment payment =
  tr [] [
    td [] [text <| toString payment.month],
    td [] [text <| prettyPrice payment.principal],
    td [] [text <| prettyPrice payment.interest],
    td [] [
      input [ type_ "number", onInput <| EarlyPrincipalItemChanged payment.month, value <| toString payment.earlyPrincipal] []
    ],
    td [] [text <| prettyPrice payment.principalBalance],
    td [] [text <| prettyPrice payment.total]
  ]

renderPaymentsHeader =
  tr [] [
    th [] [text "Месяц"],
    th [] [text "Основной долг"],
    th [] [text "Проценты"],
    th [] [text "Досрочное погашение"],
    th [] [text "Остаток кредита"],
    th [] [text "Всего"]
  ]

renderPayments model =
  div [] [
    table [] [
      tbody [] ([renderPaymentsHeader] ++ (List.map renderPayment model.payments))
    ]
  ]

renderPaymentsTotal total =
  div [class "block"] [
    br [] [],
    div [class "block-item"] [
      strong [] [text "Кредит: "],
      text <| (toString total.principal) ++ " руб"
    ],
    div [class "block-item"] [
      strong [] [text "Проценты: "],
      text <| (toString total.interest) ++ " руб"
    ],
    div [class "block-item"] [
      strong [] [text "Переплата: "],
      text <| (toString total.overpayment) ++ " %"
    ],
    div [class "block-item"] [
      strong [] [text "Эквиватентный процент: "],
      text <| (toString total.effectivePercent) ++ " %"
    ],
    div [class "block-item"] [
      strong [] [text "Сэкономлено процентов: "],
      text <| (toString total.interestSaved) ++ " руб"
    ],
    br [] []
  ]

renderDepositItem item =
  tr [] [
    td [] [text <| toString item.month],
    td [] [text <| toString item.deposit],
    td [] [text <| toString item.interest],
    td [] [text <| toString item.total]
  ]

renderDepositHistory depositHistory =
  table [class "mortgage-table"] [
    tbody [] ([
      tr [] [
        th [] [text "Месяц"],
        th [] [text "Внесено на депозит"],
        th [] [text "Проценты"],
        th [] [text "На депозите"]
      ]
    ] ++ (List.map renderDepositItem depositHistory))
  ]

renderDepositTotal total =
  div [class "block"] [
    br [] [],
    div [class "block-item"] [
      strong [] [text "Итого на вкладе: "],
      text <| (toString total.total) ++ " руб"
    ],
    div [class "block-item"] [text "."],
    div [class "block-item"] [text "."],
    div [class "block-item"] [text "."],
    div [class "block-item"] [
      strong [] [text "Получено процентов: "],
      text <| (toString total.interest) ++ " руб"
    ],
    br [] []
  ]

renderInterestComparator mortgageSavedIntereset depositInterest =
  let
    delta = abs <| roundMoney <| mortgageSavedIntereset - depositInterest
    value =
      if mortgageSavedIntereset > 0 then
        if mortgageSavedIntereset > depositInterest then
          "Досрочно погашать выгоднее на " ++ toString delta ++ " руб."
        else if mortgageSavedIntereset < depositInterest then
          "Депозит выгоднее на " ++ toString delta ++ " руб."
        else
          "Разницы нет"
      else
        ""
  in (
    div [class "block ui large header"] [
      br [] [],
      text value
    ]
  )

renderMortgageChart payments windowSize =
  let
    data = [
      ("Досрочные погашения", List.map .earlyPrincipal payments),
      ("Основной долг", List.map .principal payments),
      ("Проценты", List.map .interest payments)
    ]
  in
    Charts.StackedBarChart.render data windowSize

renderEarlyPrincipalStatChart earlyPrincipalStats windowSize =
  let
    effectivePercentLine = List.map (\item -> (item.earlyPrincipal, item.effectivePercent)) earlyPrincipalStats
    earlyPrincipalLine = List.map (\item -> (item.earlyPrincipal, toFloat item.monthCount)) earlyPrincipalStats
    labels = ("Ежемесячное досрочное погашение (руб.)", "Эквивалентный процент", "Период погашения кредита (мес.)")
  in
    Charts.LineChart.render effectivePercentLine earlyPrincipalLine windowSize labels

stylesheet path =
  let
    tag = "link"
    attrs = [
      attribute "rel" "stylesheet",
      attribute "property" "stylesheet",
      attribute "href" path
    ]
    children = []
  in
    node tag attrs children

render model =
  div [] [
    stylesheet "/main.css",
    mortgageForm model,
    renderMortgageChart model.payments model.windowSize,
    renderEarlyPrincipalStatChart model.earlyPrincipalStats model.windowSize,
    renderInterestComparator model.total.interestSaved model.depositTotal.interest,
    div [style [("float", "left")]] [
      renderPaymentsTotal model.total,
      renderPayments model
    ],
    div [style [("float", "left"), ("margin-left", "50px")]] [
      renderDepositTotal model.depositTotal,
      renderDepositHistory model.depositHistory
    ]
  ]
