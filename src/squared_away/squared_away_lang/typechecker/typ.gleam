pub type Typ {
  TNil
  TFloat
  TString
  TInt
  TBool
  TTestResult
  TUsd
  TPercent
  TDoNotEvaluate
}

pub fn to_string(typ: Typ) {
  case typ {
    TNil -> "Empty"
    TFloat -> "Floating Point Number"
    TString -> "Text"
    TInt -> "Integer"
    TBool -> "Boolean (True or False)"
    TTestResult -> "Test Result (Pass or Fail)"
    TUsd -> "Usd"
    TPercent -> "Percent"
    TDoNotEvaluate -> "Not an expression"
  }
}
