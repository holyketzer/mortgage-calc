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

type alias Model = {
  amount: Field Int,
  period: Field Int,
  interestRate: Field Float,
  earlyPrincipal: Field Int,
  depositInterest: Field Float,
  depositCapitalization: Field Capitalization,
  earlyPrincipalList: Array (Field Int),
  payments: List Payment
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
    payments = []
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
  let updatedModel =
    case converter newValue of
      Ok converted ->
        setter model (field |> setFieldValue converted newValue)
      Err message ->
        setter model (field |> setFieldError message newValue)
  in (
    { updatedModel | payments = paymentsHistory updatedModel (toFloat updatedModel.earlyPrincipal.value) }
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
      --model

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
    selectField model.depositCapitalization DepositCapitalizationChanged "Deposit capitalization" [Monthly, Yearly],
    text (toString model.depositCapitalization.value)
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

view : Model -> Html Msg
view model =
  div [] [mortgageForm model, renderPayments model]
