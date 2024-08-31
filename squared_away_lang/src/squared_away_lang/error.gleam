import squared_away_lang/interpreter/runtime_error
import squared_away_lang/parser/parse_error
import squared_away_lang/scanner/scan_error
import squared_away_lang/typechecker/type_error

pub type CompileError {
  ScanError(scan_error.ScanError)
  ParseError(parse_error.ParseError)
  TypeError(type_error.TypeError)
  RuntimeError(runtime_error.RuntimeError)
}
