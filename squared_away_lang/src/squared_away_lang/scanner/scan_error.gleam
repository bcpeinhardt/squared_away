pub type ScanError {
  ScanError(context: String)
}

pub fn to_string(se: ScanError) {
  case se {
    ScanError(txt) -> "Scan Error: " <> txt
  }
}
