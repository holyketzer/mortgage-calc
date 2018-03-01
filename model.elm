module Model exposing (Capitalization(..), FieldType(..), Field, Payment, Deposit, PaymentsTotal, DepositTotal, Model, buildField)

import Array exposing (Array)

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
