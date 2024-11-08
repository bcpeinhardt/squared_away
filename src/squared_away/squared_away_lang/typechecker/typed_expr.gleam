import bigi
import gleam/float
import gleam/int
import gleam/order
import gleam/string
import squared_away/squared_away_lang/grid
import squared_away/squared_away_lang/parser/expr
import squared_away/squared_away_lang/typechecker/typ

pub type TypedExpr {
  Empty(type_: typ.Typ)
  FloatLiteral(type_: typ.Typ, f: Float)
  UsdLiteral(type_: typ.Typ, cents: bigi.BigInt)
  PercentLiteral(
    type_: typ.Typ,
    numerator: bigi.BigInt,
    denominator: bigi.BigInt,
  )
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
  BuiltinSum(type_: typ.Typ, keys: List(grid.GridKey))
}

pub fn visit_cross_labels(
  te: TypedExpr,
  f: fn(grid.GridKey, String, String) -> Result(TypedExpr, Nil),
) -> Result(TypedExpr, Nil) {
  case te {
    CrossLabel(_, key, row, col) -> f(key, row, col)
    UnaryOp(t, o, expr) -> {
      case visit_cross_labels(expr, f) {
        Error(_) -> Error(Nil)
        Ok(modified_expr) -> Ok(UnaryOp(t, o, expr: modified_expr))
      }
    }
    BinaryOp(t, lhs, o, rhs) -> {
      case visit_cross_labels(lhs, f), visit_cross_labels(rhs, f) {
        Ok(modified_lhs), Ok(modified_rhs) ->
          Ok(BinaryOp(t, modified_lhs, o, modified_rhs))
        _, _ -> Error(Nil)
      }
    }

    _ -> Ok(te)
  }
}

fn normalize_percent(numerator: bigi.BigInt, denominator: bigi.BigInt) -> String {
  do_normalize_percent(bigi.to_string(numerator), denominator)
}

fn do_normalize_percent(n: String, d: bigi.BigInt) -> String {
  case bigi.from_int(100) |> bigi.compare(d) {
    order.Eq -> n
    order.Gt -> panic as "shouldn't happen dawg check the typed_expr module"
    order.Lt -> {
      let next_n = bigi.modulo(d, bigi.from_int(10))
      do_normalize_percent(
        n <> bigi.to_string(next_n),
        bigi.divide(d, bigi.from_int(10)),
      )
    }
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
    PercentLiteral(_, n, d) ->
      case d == bigi.from_int(100) {
        False -> normalize_percent(n, d) <> "%"
        True -> bigi.to_string(n) <> "%"
      }
    Label(_, l) -> l
    LabelDef(_, l) -> l
    Group(_, t) -> "(" <> to_string(t) <> ")"
    UnaryOp(_, op, te) -> expr.unary_to_string(op) <> to_string(te)
    BinaryOp(_, lhs, bop, rhs) ->
      to_string(lhs)
      <> " "
      <> expr.binary_to_string(bop)
      <> " "
      <> to_string(rhs)
    BuiltinSum(_, _) -> "sum"
    UsdLiteral(_, cents) -> {
      let dollars = bigi.divide(cents, bigi.from_int(100)) |> bigi.to_string
      let cents = bigi.modulo(cents, bigi.from_int(100)) |> bigi.to_string
      let cents = case string.length(cents) {
        1 -> cents <> "0"
        2 -> cents
        _ -> panic as "This shit shouldn't happen"
      }

      "$" <> dollars <> "." <> cents
    }
  }
}
