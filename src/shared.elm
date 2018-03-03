module Shared exposing (roundMoney)

roundMoney amount
  = (toFloat <| round <| amount * 100) / 100
