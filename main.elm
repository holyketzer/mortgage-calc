import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Round
import Array exposing (Array)

main =
  Html.beginnerProgram {
    model = init,
    view = view,
    update = update
  }

-- MODEL

type Capitalization = Monthly | Yearly

type FieldType = IntValue | FloatValue | TextValue | OptionValue

type alias Field a = {
  value: a, -- parsed value
  raw: String, -- raw value
  error: String,
  kind: FieldType
}

type alias Payment = {
  period: Int,
  monthlyRate: Float,
  periodicEarlyPrincipal: Float,
  earlyPrincipalList: Array Float,
  principal: Float,
  interest: Float,
  earlyPrincipal: Float,
  principalBalance: Float,
  month: Int,
  annuity: Float,
  total: Float
}

type alias Deposit = {
  deposit: Float,
  interest: Float,
  total: Float,
  month: Int,
  depositInterest: Float,
  depositCapitalization: Capitalization
}

type alias PaymentsTotal = {
  principal: Float,
  interest: Float,
  month: Int,
  overpayment: Float,
  interestSaved: Float,
  effectivePercent: Float
}

type alias DepositTotal = {
  interest: Float,
  total: Float
}

type alias Model = {
  amount: Field Int,
  period: Field Int,
  interestRate: Field Float,
  earlyPrincipal: Field Int,
  depositInterest: Field Float,
  depositCapitalization: Field Capitalization,
  earlyPrincipalList: Array (Field Int),
  payments: List Payment,
  total: PaymentsTotal,
  depositHistory: List Deposit,
  depositTotal: DepositTotal
}

buildField value kind =
  { value = value, raw = toString value, error = "", kind = kind }

init : Model
init =
  {
    amount = buildField 1000000 IntValue,
    period = buildField 60 IntValue,
    interestRate = buildField 11.0 FloatValue,
    earlyPrincipal = buildField 0 IntValue,
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
    }
  }

-- UPDATE

type Msg
  = AmountChanged String
  | PeriodChanged String
  | InterestRateChanged String
  | EarlyPrincipalChanged String
  | EarlyPrincipalItemChanged Int String
  | DepositInterestChanged String
  | DepositCapitalizationChanged String

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
  in (
    { updatedModel | payments = payments, total = total, depositHistory = deposits, depositTotal = depositTotal }
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

roundMoney amount
  = (toFloat <| round <| amount * 100) / 100

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

update : Msg -> Model -> Model
update msg model =
  case msg of
    AmountChanged value ->
      handleInput model model.amount amountSetter value String.toInt

    PeriodChanged value ->
      handleInput model model.period periodSetter value String.toInt

    InterestRateChanged value ->
      handleInput model model.interestRate interestRateSetter value String.toFloat

    EarlyPrincipalChanged value ->
      handleInput model model.earlyPrincipal earlyPrincipalSetter value String.toInt

    DepositInterestChanged value ->
      handleInput model model.depositInterest depositInterestSetter value String.toFloat

    DepositCapitalizationChanged value ->
      handleInput model model.depositCapitalization depositCapitalizationSetter value toCapitalization

    EarlyPrincipalItemChanged month value ->
      case Array.get (month - 1) model.earlyPrincipalList of
        Just field ->
          handleInput model field (earlyPrincipalListSetter (month - 1)) value String.toInt
        Nothing ->
          model

-- VIEW

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

view : Model -> Html Msg
view model =
  div [] [
    mortgageForm model,
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
