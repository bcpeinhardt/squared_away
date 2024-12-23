import gleam/option
import squared_away/squared_away_lang/grid
import squared_away/squared_away_lang/util/rational

pub type Expr {
  Empty
  FloatLiteral(f: Float)
  StringLiteral(txt: String)
  UsdLiteral(cents: rational.Rat)
  PercentLiteral(percent: rational.Rat)
  LabelDef(txt: String)
  Label(txt: String)
  CrossLabel(row: String, col: String)
  IntegerLiteral(n: Int)
  BooleanLiteral(val: Bool)
  UnaryOp(op: UnaryOpKind, expr: Expr)
  BinaryOp(lhs: Expr, op: BinaryOpKind, rhs: Expr)
  Group(inner: Expr)
  BuiltinSum(key: option.Option(grid.GridKey))
  BuiltInAvg(key: option.Option(grid.GridKey))
}

pub type BinaryOpKind {
  Add
  Subtract
  Multiply
  Divide
  Power
  EqualCheck
  NotEqualCheck
  LessThanCheck
  LessThanOrEqualCheck
  GreaterThanCheck
  GreaterThanOrEqualCheck
  And
  Or
  MustBe
  Minimum
  Maximum
}

pub fn binary_to_string(b: BinaryOpKind) -> String {
  case b {
    Add -> "+"
    And -> "&&"
    Divide -> "/"
    EqualCheck -> "=="
    GreaterThanCheck -> ">"
    GreaterThanOrEqualCheck -> ">="
    LessThanCheck -> "<"
    LessThanOrEqualCheck -> "<="
    Multiply -> "*"
    NotEqualCheck -> "!="
    Or -> "||"
    Power -> "**"
    Subtract -> "-"
    MustBe -> "mustbe"
    Minimum -> "min"
    Maximum -> "max"
  }
}

pub type UnaryOpKind {
  Negate
  Not
}

pub fn unary_to_string(u: UnaryOpKind) -> String {
  case u {
    Negate -> "-"
    Not -> "!"
  }
}
