pub type Expr {
  Empty
  FloatLiteral(f: Float)
  LabelDef(txt: String)
  Label(txt: String)
  CrossLabel(row: String, col: String)
  IntegerLiteral(n: Int)
  BooleanLiteral(val: Bool)
  UnaryOp(op: UnaryOpKind, expr: Expr)
  BinaryOp(lhs: Expr, op: BinaryOpKind, rhs: Expr)
  Group(inner: Expr)
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
