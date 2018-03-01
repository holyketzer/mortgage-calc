module Events exposing (Msg(..))

type Msg
  = AmountChanged String
  | PeriodChanged String
  | InterestRateChanged String
  | EarlyPrincipalChanged String
  | EarlyPrincipalItemChanged Int String
  | DepositInterestChanged String
  | DepositCapitalizationChanged String
