pub type Typ {
  TNil
  TFloat
  TString
  TInt
  TBool
}

pub fn to_string(typ: Typ) {
  case typ {
    TNil -> "Empty"
    TFloat -> "Floating Point Number"
    TString -> "Text"
    TInt -> "Integer"
    TBool -> "Boolean (True or False)"
  }
}
