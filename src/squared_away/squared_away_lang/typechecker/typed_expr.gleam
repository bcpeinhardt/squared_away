import gleam/float
import gleam/int
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
  BuiltinAvg(type_: typ.Typ, keys: List(grid.GridKey))
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
    Group(t, inner) -> {
      case visit_cross_labels(inner, f) {
        Error(_) -> Error(Nil)
        Ok(modified_expr) -> Ok(Group(t, expr: modified_expr))
      }
    }
    _ -> Ok(te)
  }
}

pub fn update_labels(ex: TypedExpr, old: String, new: String) -> TypedExpr {
  case ex {
    BinaryOp(t, lhs, op, rhs) ->
      BinaryOp(
        t,
        update_labels(lhs, old, new),
        op,
        update_labels(rhs, old, new),
      )
    CrossLabel(type_:, col_label:, row_label:, key:) -> {
      let row_label = case row_label == old {
        True -> new
        False -> row_label
      }

      let col_label = case col_label == old {
        True -> new
        False -> col_label
      }

      CrossLabel(type_:, col_label:, row_label:, key:)
    }
    Group(t, inner) -> Group(t, update_labels(inner, old, new))
    Label(key:, txt:, type_:) if txt == old -> Label(key:, txt: new, type_:)
    Label(_, _, _) as l -> l
    UnaryOp(t, op, inner) -> UnaryOp(t, op, update_labels(inner, old, new))
    _ -> ex
  }
}

pub fn to_string(te: TypedExpr) -> String {
  case te {
    Label(_, _, _)
    | UnaryOp(_, _, _)
    | BinaryOp(_, _, _, _)
    | Group(_, _)
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
      rational.to_string(
        rational.multiply(p, rational.from_int(100)),
        100,
        False,
      )
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
    BuiltinSum(_, _) -> "^+"
    BuiltinAvg(_, _) -> "avg"
    UsdLiteral(_, dollars) -> {
      let str = "$" <> rational.to_string(dollars, 100, False)
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
