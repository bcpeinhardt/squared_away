import gleam/option.{type Option, None, Some}

pub fn cell_to_the_right(input: String) -> Option(String) {
  case input {
    "A" <> rest -> Some("B" <> rest)
    "B" <> rest -> Some("C" <> rest)
    "C" <> rest -> Some("D" <> rest)
    "D" <> rest -> Some("E" <> rest)
    "E" <> rest -> Some("F" <> rest)
    "F" <> rest -> Some("G" <> rest)
    "G" <> rest -> Some("H" <> rest)
    "H" <> rest -> Some("I" <> rest)
    "I" <> rest -> Some("J" <> rest)
    "J" <> rest -> Some("K" <> rest)
    "K" <> rest -> Some("L" <> rest)
    "L" <> rest -> Some("M" <> rest)
    "M" <> rest -> Some("N" <> rest)
    "N" <> rest -> Some("O" <> rest)
    "O" <> rest -> Some("P" <> rest)
    "P" <> rest -> Some("Q" <> rest)
    "Q" <> rest -> Some("R" <> rest)
    "R" <> rest -> Some("S" <> rest)
    "S" <> rest -> Some("T" <> rest)
    "T" <> rest -> Some("U" <> rest)
    "U" <> rest -> Some("V" <> rest)
    "V" <> rest -> Some("W" <> rest)
    "W" <> rest -> Some("X" <> rest)
    "X" <> rest -> Some("Y" <> rest)
    "Y" <> rest -> Some("Z" <> rest)
    _ -> None
  }
}
