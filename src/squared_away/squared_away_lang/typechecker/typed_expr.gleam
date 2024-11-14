import gleam/float
import gleam/int
import gleam/list
import gleam/string
import squared_away/squared_away_lang/grid
import squared_away/squared_away_lang/parser/expr
import squared_away/squared_away_lang/typechecker/typ
import squared_away/squared_away_lang/util/rational

pub type TypedExpr {
  Empty(type_: typ.Typ)
  FloatLiteral(type_: typ.Typ, f: Float)
  UsdLiteral(type_: typ.Typ, cents: rational.Rat)
  PercentLiteral(type_: typ.Typ, percent: rational.Rat)
  Label(type_: typ.Typ, key: grid.GridKey, txt: String)
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

pub fn dependency_list(
  te: TypedExpr,
  acc: List(grid.GridKey),
) -> List(grid.GridKey) {
  case te {
    BinaryOp(_, lhs:, op: _, rhs:) ->
      list.flatten([dependency_list(lhs, []), dependency_list(rhs, []), acc])
    BooleanLiteral(_, _) -> acc
    BuiltinSum(_, keys) -> list.flatten([keys, acc])
    CrossLabel(_, key, _, _) -> [key, ..acc]
    Empty(_) -> acc
    FloatLiteral(_, _) -> acc
    Group(_, inner) -> list.flatten([dependency_list(inner, []), acc])
    IntegerLiteral(_, _) -> acc
    Label(_, key:, txt: _) -> [key, ..acc]
    LabelDef(_, _) -> acc
    PercentLiteral(_, _) -> acc
    UnaryOp(_, _, inner) -> list.flatten([dependency_list(inner, []), acc])
    UsdLiteral(_, _) -> acc
  }
}

pub fn to_string(te: TypedExpr) -> String {
  case te {
    Label(_, _, _)
    | UnaryOp(_, _, _)
    | BinaryOp(_, _, _, _)
    | BuiltinSum(_, _)
    | CrossLabel(_, _, _, _) -> "=" <> do_to_string(te)
    _ -> do_to_string(te)
  }
}

fn do_to_string(te: TypedExpr) -> String {
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
    PercentLiteral(_, p) ->
      rational.to_string(rational.multiply(p, rational.from_int(100)), 100)
      <> "%"
    Label(_, _, l) -> l
    LabelDef(_, l) -> l
    Group(_, t) -> "(" <> do_to_string(t) <> ")"
    UnaryOp(_, op, te) -> expr.unary_to_string(op) <> do_to_string(te)
    BinaryOp(_, lhs, bop, rhs) ->
      do_to_string(lhs)
      <> " "
      <> expr.binary_to_string(bop)
      <> " "
      <> do_to_string(rhs)
    BuiltinSum(_, _) -> "sum"
    UsdLiteral(_, dollars) -> {
      let str = "$" <> rational.to_string(dollars, 100)
      case string.split_once(str, ".") {
        Error(Nil) -> str <> ".00"
        Ok(#(_, cents)) -> {
          case string.length(cents) == 1 {
            False -> str
            True -> str <> "0"
          }
        }
      }
    }
  }
}
