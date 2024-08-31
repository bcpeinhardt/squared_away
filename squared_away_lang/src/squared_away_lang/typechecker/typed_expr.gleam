import squared_away_lang/parser/expr
import squared_away_lang/typechecker/typ

pub type TypedExpr {
  Empty(type_: typ.Typ)
  FloatLiteral(type_: typ.Typ, f: Float)
  Label(type_: typ.Typ, txt: String)
  LabelDef(type_: typ.Typ, txt: String, key: String)
  IntegerLiteral(type_: typ.Typ, n: Int)
  CellReference(type_: typ.Typ, key: String)
  BooleanLiteral(type_: typ.Typ, b: Bool)
  UnaryOp(type_: typ.Typ, op: expr.UnaryOpKind, expr: TypedExpr)
  BinaryOp(
    type_: typ.Typ,
    lhs: TypedExpr,
    op: expr.BinaryOpKind,
    rhs: TypedExpr,
  )
  Group(type_: typ.Typ, expr: TypedExpr)
}