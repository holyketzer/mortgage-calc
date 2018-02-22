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
  value: a,
  error: String,
  kind: FieldType
}

type alias Model = {
  amount: Field Int,
  period: Field Int,
  interestRate: Field Float,
  earlyPrincipal: Int,
  depositInterest: Float,
  depositCapitalization: Capitalization,
  earlyPrincipalList: List Int
}

init : Model
init =
  {
    amount = { value = 1000000, error = "", kind = IntValue },
    period = { value = 60, error = "", kind = IntValue },
    interestRate = { value = 11.0, error = "", kind = FloatValue },
    earlyPrincipal = 0,
    depositInterest = 6.0,
    depositCapitalization = Yearly,
    earlyPrincipalList = List.repeat 60 0
  }

-- UPDATE

type Msg
  = AmountChanged String
  | PeriodChanged String
  | InterestRateChanged String
  --| EarlyPrincipalChanged Int,
  --| EarlyPrincipalListChanged Int Int,
  --| DepositInterestChanged Float,
  --| DepositCapitalizationChanged Capitalization

setFieldValue value field =
  { field | value = value, error = "" }

setFieldError error field =
  { field | error = error }

handleInput model getter setter value converter =
  case converter value of
    Ok converted ->
      setter model (getter model |> setFieldValue converted)
    Err message ->
      setter model (getter model |> setFieldError message)

amountSetter model value =
  { model | amount = value }

periodSetter model value =
  { model | period = value }

interestRateSetter model value =
  { model | interestRate = value }

update : Msg -> Model -> Model
update msg model =
  case msg of
    AmountChanged value ->
      handleInput model .amount amountSetter value String.toInt

    PeriodChanged value ->
      handleInput model .period periodSetter value String.toInt

    InterestRateChanged value ->
      handleInput model .interestRate interestRateSetter value String.toFloat

-- VIEW

fieldType field =
  case field.kind of
    IntValue -> "number"
    _ -> "text"

renderValue field =
  case field.kind of
    IntValue ->
      toString field.value
    FloatValue ->
      toString field.value
    TextValue ->
      toString field.value

fieldInput field msg label =
  div [] [
    text (label ++ ": "),
    input [ type_ (fieldType field), placeholder label, onInput msg, value (renderValue field)] [],
    text (" " ++ field.error)
  ]

view : Model -> Html Msg
view model =
  div [] [
    fieldInput model.amount AmountChanged "Amount",
    fieldInput model.period PeriodChanged "Period",
    fieldInput model.interestRate InterestRateChanged "Interest rate",
    text (toString model.amount.value)
    --viewValidation model
  ]


--viewValidation : Model -> Html msg
--viewValidation model =
--  let
--    errors = List.filter (\(match, _) -> match) [
--      (model.password /= model.passwordAgain, "Passwords do not match!"),
--      (String.length model.password < 8, "Password should be more that 8 symbols")
--    ]
--    messages = List.map (\(_, m) -> m) errors

--    (color, message) =
--      if List.length errors == 0 then
--        ("green", "OK")
--      else
--        ("red", List.foldl (\x y -> x ++ " " ++ y) "" messages)
--  in
--    div [ style [("color", color)] ] [ text message ]
