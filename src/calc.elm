module Calc exposing (paymentsHistory, paymentsTotal, monthlyRate, getDepositHistory, getDepositTotal, effectivePercentPerEarlyPrincipal)

import Array exposing (Array)

import Model exposing (..)
import Shared exposing (roundMoney)

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

loanStateForMonth : Int -> Payment -> Payment
loanStateForMonth index prevState =
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
    loanLefts = List.scanl loanStateForMonth (initialLoanState model periodicEarlyPrincipal) monthes
  in (
    List.filter (\p -> p.total > 0) loanLefts
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

    interestAmountStep index acc =
      let
        payment = roundMoney <| annuityAmount acc.loanLeft (acc.monthCount - index) acc.monthlyRate
        interestAmount = roundMoney <| acc.loanLeft * acc.monthlyRate
        loanAmount = roundMoney <| payment - interestAmount
      in {
        loanLeft = acc.loanLeft - loanAmount,
        totalInterest = acc.totalInterest + interestAmount,
        monthCount = acc.monthCount,
        monthlyRate = acc.monthlyRate
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
  in (percentByInterestLoop amount monthCount totalInterest left right delta) * 12 * 100

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
    newInterest =
      if payment.month > 1 then
        roundMoney (prevDeposit.total * prevDeposit.depositInterest / 12 / 100)
      else
        0

    (interestToCapitalize, interest) =
      case prevDeposit.depositCapitalization of
        Monthly ->
          (0, newInterest)
        Yearly ->
          if payment.month % 12 == 0 then
            (0, roundMoney <| prevDeposit.interestToCapitalize + newInterest)
          else
            (roundMoney <| prevDeposit.interestToCapitalize + newInterest, 0)

    total = roundMoney (prevDeposit.total + payment.earlyPrincipal + interest)
  in (
    {
      deposit = payment.earlyPrincipal,
      interest = interest,
      interestToCapitalize = interestToCapitalize,
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
      interestToCapitalize = 0,
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
