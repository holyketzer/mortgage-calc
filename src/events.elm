module Events exposing (Msg(..))

import Window

type Msg
  = GetWindowSize Window.Size
  | AmountChanged String
  | PeriodChanged String
  | InterestRateChanged String
  | EarlyPrincipalChanged String
  | EarlyPrincipalItemChanged Int String
  | DepositInterestChanged String
  | DepositCapitalizationChanged String
