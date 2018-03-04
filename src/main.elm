import Array exposing (Array)
import Html exposing (..)
import Round

import Events exposing (..)
import Model exposing (..)
import View exposing (render)
import Shared exposing (roundMoney)
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

monthlyRate interestRate =
  interestRate / (12.0 * 100)

annuityAmount loanAmount monthCount monthlyRate =
  roundMoney ((loanAmount * monthlyRate) / (1 - ((1 + monthlyRate) ^ toFloat(-monthCount))))

initialLoanState : Model -> Float -> Payment
initialLoanState model periodicEarlyPrincipal =
  {
    period = model.period.value,
    monthlyRate = monthlyRate model.interestRate.value,
    periodicEarlyPrincipal = periodicEarlyPrincipal,
    earlyPrincipalList = Array.map (\f -> toFloat f.value) model.earlyPrincipalList,
    principal = 0,
    interest = 0,
    earlyPrincipal = 0,
    principalBalance = toFloat model.amount.value,
    month = model.period.value,
    annuity = 0,
    total = 0
  }

loanStateFor : Int -> Payment -> Payment
loanStateFor index prevState =
  let
    payment = annuityAmount prevState.principalBalance (prevState.period - index) prevState.monthlyRate
    interestAmount = roundMoney (prevState.principalBalance * prevState.monthlyRate)
    loanAmount = roundMoney (payment - interestAmount)

    loanLeftAfterAnnuity = prevState.principalBalance - loanAmount

    specificEarlyPrincipal = Array.get index prevState.earlyPrincipalList

    currentEarlyPrincipal =
      case specificEarlyPrincipal of
        Just value ->
          if value > 0 then value else prevState.periodicEarlyPrincipal
        Nothing ->
          prevState.periodicEarlyPrincipal

    earlyPrincipal = roundMoney (Basics.min loanLeftAfterAnnuity currentEarlyPrincipal)
    loanLeft = roundMoney (loanLeftAfterAnnuity - earlyPrincipal)
  in (
    {
      period = prevState.period,
      monthlyRate = prevState.monthlyRate,
      periodicEarlyPrincipal = prevState.periodicEarlyPrincipal,
      earlyPrincipalList = prevState.earlyPrincipalList,
      principal = loanAmount,
      interest = interestAmount,
      earlyPrincipal = earlyPrincipal,
      principalBalance = loanLeft,
      month = index + 1,
      annuity = payment,
      total = roundMoney (loanAmount + interestAmount + earlyPrincipal)
    }
  )

paymentsHistory : Model -> Float -> List Payment
paymentsHistory model periodicEarlyPrincipal =
  let
    loanLeft = model.amount.value
    monthes = List.range 0 (model.period.value - 1)
    loanLefts = List.scanl loanStateFor (initialLoanState model periodicEarlyPrincipal) monthes
  in (
    List.filter (\p -> p.total > 0) loanLefts
  )

interestAmountStep index acc =
  let
    payment = roundMoney <| annuityAmount acc.loanLeft (acc.monthCount - index) acc.monthlyRate
    interestAmount = roundMoney <| acc.loanLeft * acc.monthlyRate
    loanAmount = roundMoney <| payment - interestAmount
  in (
    {
      loanLeft = acc.loanLeft - loanAmount,
      totalInterest = acc.totalInterest + interestAmount,
      monthCount = acc.monthCount,
      monthlyRate = acc.monthlyRate
    }
  )

interestAmount : Float -> Float -> Int -> Float
interestAmount amount monthlyRate monthCount =
  let
    from = {
      loanLeft = amount,
      totalInterest = 0,
      monthCount = monthCount,
      monthlyRate = monthlyRate
    }

    res = List.foldl interestAmountStep from (List.range 0 (monthCount - 1))
  in (
    res.totalInterest
  )

percentByInterest : Float -> Float -> Float -> Int -> Float
percentByInterest amount monthlyRate totalInterest monthCount =
  let
    left = 0
    right = monthlyRate
    delta = amount
  in (
    (percentByInterestLoop amount monthCount totalInterest left right delta) * 12 * 100
  )

percentByInterestLoop : Float -> Int -> Float -> Float -> Float -> Float -> Float
percentByInterestLoop amount monthCount totalInterest left right delta =
  if delta > 10 then
    let
      newCurrent = (right + left) / 2
      newInterest = interestAmount amount newCurrent monthCount
      newDelta = abs (newInterest - totalInterest)
    in (
      if newInterest > totalInterest then
        percentByInterestLoop amount monthCount totalInterest left newCurrent newDelta
      else
        percentByInterestLoop amount monthCount totalInterest newCurrent right newDelta
    )
  else
    left

paymentsTotal : List Payment -> Float -> Float -> Int -> PaymentsTotal
paymentsTotal payments amount monthlyRate period =
  let
    principal = roundMoney <| List.sum <| (List.map .principal payments) ++ (List.map .earlyPrincipal payments)
    interest = roundMoney <| List.sum <| (List.map .interest payments)
    month = List.length payments

    interestSaved = (interestAmount amount monthlyRate period) - interest
  in (
    {
      principal = principal,
      interest = interest,
      month = month,
      overpayment = roundMoney (100 * interest / principal),
      interestSaved = roundMoney interestSaved,
      effectivePercent = roundMoney (percentByInterest amount monthlyRate interest month)
    }
  )

getDepositHistoryItem : Payment -> Deposit -> Deposit
getDepositHistoryItem payment prevDeposit =
  let
    interest =
      if payment.month > 1 then
        case prevDeposit.depositCapitalization of
          Monthly ->
            roundMoney (prevDeposit.total * prevDeposit.depositInterest / 12 / 100)
          Yearly ->
            if payment.month % 12 == 0 then
              roundMoney (prevDeposit.total * prevDeposit.depositInterest / 100)
            else
              0
      else
        0

    total = roundMoney (prevDeposit.total + payment.earlyPrincipal + interest)
  in (
    {
      deposit = payment.earlyPrincipal,
      interest = interest,
      total = total,
      month = payment.month,
      depositInterest = prevDeposit.depositInterest,
      depositCapitalization = prevDeposit.depositCapitalization
    }
  )

getDepositHistory : List Payment -> Float -> Capitalization -> List Deposit
getDepositHistory paymentsHistory depositInterest depositCapitalization =
  let
    initialDeposit = {
      deposit = 0,
      interest = 0,
      total = 0,
      month = 0,
      depositInterest = depositInterest,
      depositCapitalization = depositCapitalization
    }
    deposits = List.scanl getDepositHistoryItem initialDeposit paymentsHistory
  in (
    List.filter (\deposit -> deposit.month > 0) deposits
  )

getDepositTotal : List Deposit -> DepositTotal
getDepositTotal depositHistory =
  let
    lastItem = List.head <| List.reverse depositHistory
    total =
      case lastItem of
        Just deposit -> deposit.total
        Nothing -> 0
  in (
    {
      interest = roundMoney <| List.sum <| List.map .interest depositHistory,
      total = total
    }
  )

effectivePercentPerEarlyPrincipal : Model -> Float -> List EarlyPrincipalStat
effectivePercentPerEarlyPrincipal model annuity =
  let
    step earlyPrincipal =
      if earlyPrincipal < 10000 then
        500
      else if earlyPrincipal < 100000 then
        1000
      else if earlyPrincipal < 500000 then
        5000
      else
        10000

    amount = toFloat model.amount.value
    rate = monthlyRate model.interestRate.value

    getStat : Float -> List EarlyPrincipalStat -> List EarlyPrincipalStat
    getStat prevEarlyPrincipal res =
      if prevEarlyPrincipal <= annuity * 3 then
        let
          earlyPrincipal = prevEarlyPrincipal + (step prevEarlyPrincipal)
          payments = paymentsHistory model earlyPrincipal
          monthCount = List.length payments
          interest = List.sum <| List.map .interest payments
          effectivePercent = roundMoney <| percentByInterest amount rate interest monthCount

          item = {
            earlyPrincipal = earlyPrincipal,
            effectivePercent = effectivePercent,
            monthCount = monthCount
          }
        in
          getStat earlyPrincipal (item :: res)
      else
        res
  in
    List.reverse <| getStat 0 []

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
