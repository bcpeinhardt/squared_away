import gleam/dict
import gleam/list.{Continue, Stop}
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import squared_away_lang/error
import squared_away_lang/parser/expr
import squared_away_lang/typechecker/typ
import squared_away_lang/typechecker/type_error
import squared_away_lang/typechecker/typed_expr
import squared_away_lang/util

pub fn typecheck(
  env: dict.Dict(String, Result(expr.Expr, error.CompileError)),
  expr: expr.Expr,
) -> Result(typed_expr.TypedExpr, error.CompileError) {
  case expr {
    expr.Empty -> Ok(typed_expr.Empty(type_: typ.TNil))
    expr.LabelDef(txt) -> Ok(typed_expr.LabelDef(type_: typ.TNil, txt:))

    // We will typecheck the label when we typecheck the grid as a whole. For now it's a 
    // "Nil" type
    expr.Label(txt) -> {
      let key =
        env
        |> dict.to_list
        |> list.fold_until(None, fn(_, i) {
          case i {
            #(cell_ref, Ok(expr.LabelDef(label_txt))) if label_txt == txt -> {
              Stop(Some(cell_ref))
            }
            _ -> Continue(None)
          }
        })
        |> option.map(util.cell_to_the_right)
        |> option.flatten

      case key {
        None -> Ok(typed_expr.Label(typ.TNil, txt))
        Some(key) -> {
          let assert Ok(x) = dict.get(env, key)
          case x {
            Error(e) -> Error(e)
            Ok(expr) -> {
              case typecheck(env, expr) {
                Ok(te) -> Ok(typed_expr.Label(type_: te.type_, txt:))
                Error(e) -> Error(e)
              }
            }
          }
        }
      }
    }
    expr.CrossLabel(row, col) -> {
      let assert Some(col_cell) =
        env
        |> dict.to_list
        |> list.fold_until(None, fn(_, i) {
          case i {
            #(cell_ref, Ok(expr.LabelDef(label_txt))) if label_txt == col -> {
              Stop(Some(cell_ref))
            }
            _ -> Continue(None)
          }
        })
      let col =
        col_cell
        |> string.to_graphemes
        |> list.filter(string.contains("ABCDEFGHIJKLMNOPQRSTUVWXYZ", _))
        |> string.join("")

      let assert Some(row_cell) =
        env
        |> dict.to_list
        |> list.fold_until(None, fn(_, i) {
          case i {
            #(cell_ref, Ok(expr.LabelDef(label_txt))) if label_txt == row -> {
              Stop(Some(cell_ref))
            }
            _ -> Continue(None)
          }
        })

      let row =
        row_cell
        |> string.to_graphemes
        |> list.filter(string.contains("0123456789", _))
        |> string.join("")
      let key = col <> row

      let assert Ok(x) = dict.get(env, key)
      case x {
        Error(e) -> Error(e)
        Ok(expr) -> {
          case typecheck(env, expr) {
            Ok(te) -> Ok(typed_expr.CrossLabel(type_: te.type_, key:))
            Error(e) -> Error(e)
          }
        }
      }
    }
    expr.BooleanLiteral(b) ->
      Ok(typed_expr.BooleanLiteral(type_: typ.TBool, b:))
    expr.FloatLiteral(f) -> Ok(typed_expr.FloatLiteral(type_: typ.TFloat, f:))
    expr.IntegerLiteral(n) -> Ok(typed_expr.IntegerLiteral(type_: typ.TInt, n:))
    expr.Group(inner) -> {
      use expr <- result.try(typecheck(env, inner))
      Ok(typed_expr.Group(type_: expr.type_, expr:))
    }
    expr.UnaryOp(op, expr) -> {
      use expr <- result.try(typecheck(env, expr))
      case op, expr.type_ {
        expr.Negate, typ.TInt | expr.Negate, typ.TFloat ->
          Ok(typed_expr.UnaryOp(type_: expr.type_, op:, expr:))
        expr.Not, typ.TBool ->
          Ok(typed_expr.UnaryOp(type_: expr.type_, op:, expr:))
        _, _ ->
          Error(
            error.TypeError(type_error.TypeError(
              "Unexpected type and operator combination",
            )),
          )
      }
    }
    expr.BinaryOp(lhs, op, rhs) -> {
      use lhs <- result.try(typecheck(env, lhs))
      use rhs <- result.try(typecheck(env, rhs))
      case lhs.type_, op, rhs.type_ {
        // Addition
        typ.TFloat, expr.Add, typ.TFloat ->
          Ok(typed_expr.BinaryOp(type_: typ.TFloat, lhs:, op:, rhs:))
        typ.TInt, expr.Add, typ.TInt ->
          Ok(typed_expr.BinaryOp(type_: typ.TInt, lhs:, op:, rhs:))

        // Subtraction 
        typ.TFloat, expr.Subtract, typ.TFloat ->
          Ok(typed_expr.BinaryOp(type_: typ.TFloat, lhs:, op:, rhs:))
        typ.TInt, expr.Subtract, typ.TInt ->
          Ok(typed_expr.BinaryOp(type_: typ.TInt, lhs:, op:, rhs:))

        // Multiplication
        typ.TFloat, expr.Multiply, typ.TFloat ->
          Ok(typed_expr.BinaryOp(type_: typ.TFloat, lhs:, op:, rhs:))
        typ.TInt, expr.Multiply, typ.TInt ->
          Ok(typed_expr.BinaryOp(type_: typ.TInt, lhs:, op:, rhs:))

        // Division
        typ.TFloat, expr.Divide, typ.TFloat ->
          Ok(typed_expr.BinaryOp(type_: typ.TFloat, lhs:, op:, rhs:))
        typ.TInt, expr.Divide, typ.TInt ->
          Ok(typed_expr.BinaryOp(type_: typ.TInt, lhs:, op:, rhs:))

        // Power
        typ.TFloat, expr.Power, typ.TFloat | typ.TInt, expr.Power, typ.TFloat ->
          Ok(typed_expr.BinaryOp(type_: typ.TFloat, lhs:, op:, rhs:))

        // Equal and Not Equal Check 
        t1, expr.EqualCheck, t2 | t1, expr.NotEqualCheck, t2 if t1 == t2 ->
          Ok(typed_expr.BinaryOp(type_: typ.TBool, lhs:, op:, rhs:))

        // Ordering Checks
        typ.TFloat, expr.LessThanCheck, typ.TFloat
        | typ.TFloat, expr.LessThanOrEqualCheck, typ.TFloat
        | typ.TFloat, expr.GreaterThanOrEqualCheck, typ.TFloat
        | typ.TFloat, expr.GreaterThanCheck, typ.TFloat
        | typ.TInt, expr.LessThanCheck, typ.TInt
        | typ.TInt, expr.LessThanOrEqualCheck, typ.TInt
        | typ.TInt, expr.GreaterThanOrEqualCheck, typ.TInt
        | typ.TInt, expr.GreaterThanCheck, typ.TInt
        | typ.TString, expr.LessThanCheck, typ.TString
        | typ.TString, expr.LessThanOrEqualCheck, typ.TString
        | typ.TString, expr.GreaterThanOrEqualCheck, typ.TString
        | typ.TString, expr.GreaterThanCheck, typ.TString
        -> Ok(typed_expr.BinaryOp(type_: typ.TBool, lhs:, op:, rhs:))

        // Boolean Operations
        typ.TBool, expr.And, typ.TBool | typ.TBool, expr.Or, typ.TBool ->
          Ok(typed_expr.BinaryOp(type_: typ.TBool, lhs:, op:, rhs:))

        // Boolean TypeErrors
        typ.TBool, expr.And, t ->
          case t {
            typ.TNil ->
              Error(
                error.TypeError(type_error.TypeError(
                  "Tried to do a boolean and operation \"&&\" but the right hand side of the operation has type \"Empty\". Could you be referencing an empty cell?",
                )),
              )
            _ ->
              Error(
                error.TypeError(type_error.TypeError(
                  "Tried to do a boolean and operation (b1 && b2) but the right hand side has type "
                  <> string.inspect(t),
                )),
              )
          }

        _, _, _ ->
          Error(
            error.TypeError(type_error.TypeError(
              "Unexpected arguments to binary operation: " <> string.inspect(op),
            )),
          )
      }
    }
  }
}
