pub type ParseError {
  ParseError(context: String)
}

pub fn to_string(pe: ParseError) -> String {
  let ParseError(c) = pe
  c
}
