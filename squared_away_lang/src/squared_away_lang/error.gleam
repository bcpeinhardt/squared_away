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

pub fn to_string(e: CompileError) -> String {
  case e {
    ParseError(parse_error.ParseError(txt)) ->
      error_type_string(e) <> ": " <> txt
    RuntimeError(runtime_error.RuntimeError(txt)) ->
      error_type_string(e) <> ": " <> txt
    ScanError(scan_error.ScanError) ->
      error_type_string(e) <> ": Unrecognized token"
    TypeError(te) -> type_error.to_string(te)
  }
}

pub fn error_type_string(e: CompileError) -> String {
  case e {
    ParseError(_) -> "Parse Error"
    RuntimeError(_) -> "Runtime Error"
    ScanError(_) -> "Scan Error"
    TypeError(_) -> "Type Error"
  }
}
