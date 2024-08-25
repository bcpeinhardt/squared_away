import gleam/dict
import squared_away/lang/parser

/// Our very basic starting environment
pub type Environment =
  dict.Dict(String, parser.Expr)
