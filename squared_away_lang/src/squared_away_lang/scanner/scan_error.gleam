pub type ScanError {
  ScanError
}

pub fn to_string(se: ScanError) {
  case se {
    ScanError -> "Scan Error"
  }
}
