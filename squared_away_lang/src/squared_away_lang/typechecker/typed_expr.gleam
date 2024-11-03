import gleam/float
import gleam/int
import squared_away_lang/grid
import squared_away_lang/parser/expr
import squared_away_lang/typechecker/typ

pub type TypedExpr {
  Empty(type_: typ.Typ)
  FloatLiteral(type_: typ.Typ, f: Float)
  Label(type_: typ.Typ, txt: String)
  CrossLabel(
    type_: typ.Typ,
    key: grid.GridKey,
    row_label: String,
    col_label: String,
  )
  LabelDef(type_: typ.Typ, txt: String)
  IntegerLiteral(type_: typ.Typ, n: Int)
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

pub fn visit_cross_labels(
  te: TypedExpr,
  f: fn(grid.GridKey, String, String) -> TypedExpr,
) -> TypedExpr {
  case te {
    CrossLabel(_, key, row, col) -> f(key, row, col)
    UnaryOp(t, o, expr) -> UnaryOp(t, o, expr: visit_cross_labels(expr, f))
    BinaryOp(t, lhs, o, rhs) ->
      BinaryOp(t, visit_cross_labels(lhs, f), o, visit_cross_labels(rhs, f))
    _ -> te
  }
}

pub fn to_string(te: TypedExpr) -> String {
  case te {
    BooleanLiteral(_, b) -> {
      case b {
        False -> "FALSE"
        True -> "TRUE"
      }
    }
    CrossLabel(_, _, rl, cl) -> rl <> "_" <> cl
    Empty(_) -> ""
    FloatLiteral(_, f) -> float.to_string(f)
    IntegerLiteral(_, i) -> int.to_string(i)
    Label(_, l) -> l
    LabelDef(_, l) -> l
    Group(_, t) -> "(" <> to_string(t) <> ")"
    UnaryOp(_, op, te) -> expr.unary_to_string(op) <> to_string(te)
    BinaryOp(_, lhs, bop, rhs) ->
      to_string(lhs) <> expr.binary_to_string(bop) <> to_string(rhs)
  }
}
