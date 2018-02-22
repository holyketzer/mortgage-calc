import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
  Html.beginnerProgram {
    model = init,
    view = view,
    update = update
  }

-- MODEL

type Capitalization = Monthly | Yearly

type FieldType = IntValue | FloatValue | TextValue

type alias Field a = {
  value: a, -- parsed value
  raw: String, -- raw value
  error: String,
  kind: FieldType
}

type alias Model = {
  amount: Field Int,
  period: Field Int,
  interestRate: Field Float,
  earlyPrincipal: Field Int,
  depositInterest: Field Float,
  depositCapitalization: Capitalization,
  earlyPrincipalList: List Int
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
    depositCapitalization = Yearly,
    earlyPrincipalList = List.repeat 60 0
  }

-- UPDATE

type Msg
  = AmountChanged String
  | PeriodChanged String
  | InterestRateChanged String
  | EarlyPrincipalChanged String
  --| EarlyPrincipalListChanged Int Int
  | DepositInterestChanged String
  --| DepositCapitalizationChanged Capitalization

setFieldValue value raw field =
  { field | value = value, raw = raw, error = "" }

setFieldError error raw field =
  { field | error = error, raw = raw }

handleInput model field setter newValue converter =
  case converter newValue of
    Ok converted ->
      setter model (field |> setFieldValue converted newValue)
    Err message ->
      setter model (field |> setFieldError message newValue)

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

-- VIEW

fieldType field =
  case field.kind of
    IntValue -> "number"
    _ -> "text"

fieldInput field msg label =
  div [] [
    text (label ++ ": "),
    input [ type_ (fieldType field), placeholder label, onInput msg, value field.raw] [],
    text (" " ++ field.error)
  ]

view : Model -> Html Msg
view model =
  div [] [
    fieldInput model.amount AmountChanged "Amount",
    fieldInput model.period PeriodChanged "Period",
    fieldInput model.interestRate InterestRateChanged "Interest rate",
    fieldInput model.earlyPrincipal EarlyPrincipalChanged "Early principal",
    fieldInput model.depositInterest DepositInterestChanged "Desposit interest",
    text (toString model.amount.value)
  ]
