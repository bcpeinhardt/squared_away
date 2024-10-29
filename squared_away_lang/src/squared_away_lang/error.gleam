import renderable_error
import squared_away_lang/interpreter/runtime_error
import squared_away_lang/parser/parse_error
import squared_away_lang/scanner/scan_error
import squared_away_lang/typechecker/type_error
import gleam/option.{None}

pub type CompileError {
  ScanError(scan_error.ScanError)
  ParseError(parse_error.ParseError)
  TypeError(type_error.TypeError)
  RuntimeError(runtime_error.RuntimeError)
}

pub fn to_renderable_error(ce: CompileError) -> renderable_error.RenderableError {
  case ce {
    TypeError(te) -> type_error.to_renderable_error(te)
    _ -> renderable_error.RenderableError(title: "Compiler error", info: "Todo: implement this error description", hint: None)
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
