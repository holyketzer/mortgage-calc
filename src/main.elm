import Array exposing (Array)
import Html exposing (..)
import Round

import Calc exposing (..)
import Events exposing (..)
import Model exposing (..)
import View exposing (render)
import Task
import Window

main =
  Html.program {
    init = (
      init,
      Cmd.batch [
        Task.perform (\size -> GetWindowSize size) Window.size,
        Task.perform (\_ -> InterestRateChanged "11.0") (Task.succeed ())
      ]
    ),
    view = view,
    update = update,
    subscriptions = subscriptions
  }

-- MODEL

init : Model
init =
  {
    windowSize = { width = 100, height = 50 },
    amount = buildField 1000000 IntValue,
    period = buildField 60 IntValue,
    interestRate = buildField 11.0 FloatValue,
    earlyPrincipal = buildField 10000 IntValue,
    depositInterest = buildField 6.0 FloatValue,
    depositCapitalization = buildField Yearly OptionValue,
    earlyPrincipalList = Array.repeat 60 (buildField 0 IntValue),
    payments = [],
    total = {
      principal = 0,
      interest = 0,
      month = 0,
      overpayment = 0,
      interestSaved = 0,
      effectivePercent = 0
    },
    depositHistory = [],
    depositTotal = {
      interest = 0,
      total = 0
    },
    earlyPrincipalStats = []
  }

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- UPDATE

setFieldValue value raw field =
  { field | value = value, raw = raw, error = "" }

setFieldError error raw field =
  { field | error = error, raw = raw }

handleInput model field setter newValue converter =
  let
    updatedModel =
      case converter newValue of
        Ok converted ->
          setter model (field |> setFieldValue converted newValue)
        Err message ->
          setter model (field |> setFieldError message newValue)

    payments = paymentsHistory updatedModel (toFloat updatedModel.earlyPrincipal.value)
    total = paymentsTotal payments (toFloat updatedModel.amount.value) (monthlyRate updatedModel.interestRate.value) updatedModel.period.value
    deposits = getDepositHistory payments updatedModel.depositInterest.value updatedModel.depositCapitalization.value
    depositTotal = getDepositTotal deposits

    earlyPrincipalStats =
      case List.head payments of
        Just payment ->
          effectivePercentPerEarlyPrincipal updatedModel payment.annuity
        Nothing ->
          []
  in (
    {
      updatedModel |
        payments = payments,
        total = total,
        depositHistory = deposits,
        depositTotal = depositTotal,
        earlyPrincipalStats = earlyPrincipalStats
    }
  )

amountSetter model value =
  { model | amount = value }

periodSetter model value =
  { model | period = value }

interestRateSetter model value =
  { model | interestRate = value }

earlyPrincipalSetter model value =
  { model | earlyPrincipal = value }

depositInterestSetter model value =
  { model | depositInterest = value }

depositCapitalizationSetter model value =
  { model | depositCapitalization = value }

earlyPrincipalListSetter index model value =
  { model | earlyPrincipalList = Array.set index value model.earlyPrincipalList }

toCapitalization value =
  case value of
    "Yearly" -> Ok Yearly
    "Monthly" -> Ok Monthly
    _ -> Err "wrong value"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetWindowSize size ->
      ({ model | windowSize = size }, Cmd.none)

    AmountChanged value ->
      (handleInput model model.amount amountSetter value String.toInt, Cmd.none)

    PeriodChanged value ->
      (handleInput model model.period periodSetter value String.toInt, Cmd.none)

    InterestRateChanged value ->
      (handleInput model model.interestRate interestRateSetter value String.toFloat, Cmd.none)

    EarlyPrincipalChanged value ->
      (handleInput model model.earlyPrincipal earlyPrincipalSetter value String.toInt, Cmd.none)

    DepositInterestChanged value ->
      (handleInput model model.depositInterest depositInterestSetter value String.toFloat, Cmd.none)

    DepositCapitalizationChanged value ->
      (handleInput model model.depositCapitalization depositCapitalizationSetter value toCapitalization, Cmd.none)

    EarlyPrincipalItemChanged month value ->
      case Array.get (month - 1) model.earlyPrincipalList of
        Just field ->
          (handleInput model field (earlyPrincipalListSetter (month - 1)) value String.toInt, Cmd.none)
        Nothing ->
          (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  View.render model
