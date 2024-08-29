import squared_away/lang/interpreter/runtime_error
import squared_away/lang/parser/parse_error
import squared_away/lang/scanner/scan_error
import squared_away/lang/typechecker/type_error

pub type CompileError {
  ScanError(scan_error.ScanError)
  ParseError(parse_error.ParseError)
  TypeError(type_error.TypeError)
  RuntimeError(runtime_error.RuntimeError)
}
