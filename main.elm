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

type FieldType = IntValue | FloatValue | TextValue | OptionValue

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
  depositCapitalization: Field Capitalization,
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
    depositCapitalization = buildField Yearly OptionValue,
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
  | DepositCapitalizationChanged String

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

depositCapitalizationSetter model value =
  { model | depositCapitalization = value }

toCapitalization value =
  case value of
    "Yearly" -> Ok Yearly
    "Monthly" -> Ok Monthly
    _ -> Err "wrong value"

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

view : Model -> Html Msg
view model =
  div [] [
    inputField model.amount AmountChanged "Amount",
    inputField model.period PeriodChanged "Period",
    inputField model.interestRate InterestRateChanged "Interest rate",
    inputField model.earlyPrincipal EarlyPrincipalChanged "Early principal",
    inputField model.depositInterest DepositInterestChanged "Desposit interest",
    selectField model.depositCapitalization DepositCapitalizationChanged "Deposit capitalization" [Monthly, Yearly],
    text (toString model.depositCapitalization.value)
  ]
