pub type Expr {
  Empty
  FloatLiteral(f: Float)
  LabelDef(txt: String)
  Label(txt: String)
  CrossLabel(row: String, col: String)
  IntegerLiteral(n: Int)
  CellReference(key: String)
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

pub type UnaryOpKind {
  Negate
  Not
}
