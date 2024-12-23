import gleam/option.{None}
import gleam/string
import squared_away/renderable_error
import squared_away/squared_away_lang/interpreter/runtime_error
import squared_away/squared_away_lang/parser/parse_error
import squared_away/squared_away_lang/scanner/scan_error
import squared_away/squared_away_lang/typechecker/type_error

pub type CompileError {
  ScanError(scan_error.ScanError)
  ParseError(parse_error.ParseError)
  TypeError(type_error.TypeError)
  RuntimeError(runtime_error.RuntimeError)
}

pub fn to_renderable_error(ce: CompileError) -> renderable_error.RenderableError {
  case ce {
    TypeError(te) -> type_error.to_renderable_error(te)
    ParseError(pe) ->
      renderable_error.RenderableError(title: "", info: pe.context, hint: None)
    RuntimeError(re) ->
      renderable_error.RenderableError(title: "", info: re.context, hint: None)
    _ ->
      renderable_error.RenderableError(
        title: "Compiler error",
        info: string.inspect(ce),
        hint: None,
      )
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
