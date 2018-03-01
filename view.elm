module View exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Round
import Charts.StackedBarChart

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

someToOption x current =
  option [value (toString x), selected (x == current)] [text (toString x)]

mortgageForm model =
  div [] [
    inputField model.amount AmountChanged "Amount",
    inputField model.period PeriodChanged "Period",
    inputField model.interestRate InterestRateChanged "Interest rate",
    inputField model.earlyPrincipal EarlyPrincipalChanged "Early principal",
    inputField model.depositInterest DepositInterestChanged "Desposit interest",
    selectField model.depositCapitalization DepositCapitalizationChanged "Deposit capitalization" [Monthly, Yearly]
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
    th [] [text "Month"],
    th [] [text "Principal"],
    th [] [text "Interest"],
    th [] [text "Early principal"],
    th [] [text "Principal balance"],
    th [] [text "Total"]
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
      strong [] [text "Total principal: "],
      text <| (toString total.principal) ++ " руб"
    ],
    div [class "block-item"] [
      strong [] [text "Total interest: "],
      text <| (toString total.interest) ++ " руб"
    ],
    div [class "block-item"] [
      strong [] [text "Overpayment: "],
      text <| (toString total.overpayment) ++ " %"
    ],
    div [class "block-item"] [
      strong [] [text "Effective percent: "],
      text <| (toString total.effectivePercent) ++ " %"
    ],
    div [class "block-item"] [
      strong [] [text "Interest saved: "],
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
        th [] [text "Month"],
        th [] [text "Deposit"],
        th [] [text "Interest"],
        th [] [text "Total"]
      ]
    ] ++ (List.map renderDepositItem depositHistory))
  ]

renderDepositTotal total =
  div [class "block"] [
    br [] [],
    div [class "block-item"] [
      strong [] [text "Total: "],
      text <| (toString total.total) ++ " руб"
    ],
    div [class "block-item"] [text "."],
    div [class "block-item"] [text "."],
    div [class "block-item"] [text "."],
    div [class "block-item"] [
      strong [] [text "Interest earned: "],
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

renderMortgageChart payments =
  let
    data = [
      ("Early principal", List.map .earlyPrincipal payments),
      ("Principal", List.map .principal payments),
      ("Interest", List.map .interest payments)
    ]
  in
    Charts.StackedBarChart.render data

samples =
  [
    ("Assault", [1, 2, 3, 4, 5, 6, 7]),
    ("Robbery", [3, 5, 4, 3, 5, 4, 2])
  ]

render model =
  div [] [
    mortgageForm model,
    renderMortgageChart model.payments,
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
