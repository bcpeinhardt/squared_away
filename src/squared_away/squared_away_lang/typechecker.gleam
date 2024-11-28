import gleam/bool
import gleam/int
import gleam/list.{Continue, Stop}
import gleam/option.{None, Some}
import gleam/result
import squared_away/squared_away_lang/error
import squared_away/squared_away_lang/grid
import squared_away/squared_away_lang/parser/expr
import squared_away/squared_away_lang/typechecker/typ
import squared_away/squared_away_lang/typechecker/type_error
import squared_away/squared_away_lang/typechecker/typed_expr

pub fn typecheck(
  env: grid.Grid(Result(expr.Expr, error.CompileError)),
  expr: expr.Expr,
) -> Result(typed_expr.TypedExpr, error.CompileError) {
  case expr {
    expr.Empty -> Ok(typed_expr.Empty(type_: typ.TNil))
    expr.BuiltinSum(key) -> {
      let assert option.Some(key) = key

      // The sum builtin will sum up all the values above it until it finds a label, so we need to fetch those 
      // values and sort them
      let items_above_sum_call =
        grid.to_list(env)
        |> list.filter(fn(i) {
          let #(gk, _) = i
          grid.col(gk) == grid.col(key) && grid.row(gk) < grid.row(key)
        })
        |> list.sort(fn(i1, i2) {
          let #(gk1, _) = i1
          let #(gk2, _) = i2
          int.compare(grid.row(gk1), grid.row(gk2))
        })
        |> list.reverse
        |> list.take_while(fn(i) {
          let #(_, item) = i
          case item {
            Ok(expr.LabelDef(_)) -> False
            _ -> True
          }
        })

      let keys = list.map(items_above_sum_call, fn(i) { i.0 })
      let items_above_sum_call = list.map(items_above_sum_call, fn(i) { i.1 })

      use <- bool.guard(
        list.any(items_above_sum_call, result.is_error),
        Error(
          error.TypeError(type_error.TypeError(
            "Cell above sum expression has error",
          )),
        ),
      )

      let types =
        list.map(items_above_sum_call, fn(i) {
          let assert Ok(item) = i
          item
        })
        |> list.unique
        |> list.map(fn(e) { typecheck(env, e) })

      use <- bool.guard(
        list.any(types, result.is_error),
        Error(
          error.TypeError(type_error.TypeError(
            "Cell above sum expression has type error",
          )),
        ),
      )

      let types =
        list.map(types, fn(t) {
          let assert Ok(texpr) = t
          texpr.type_
        })
        |> list.unique
        |> list.filter(fn(t) { t != typ.TTestResult && t != typ.TNil })

      case types {
        [typ.TFloat] -> Ok(typed_expr.BuiltinSum(typ.TFloat, keys))
        [typ.TInt] -> Ok(typed_expr.BuiltinSum(typ.TInt, keys))
        [typ.TUsd] -> Ok(typed_expr.BuiltinSum(typ.TUsd, keys))
        [typ.TPercent] -> Ok(typed_expr.BuiltinSum(typ.TPercent, keys))
        _ ->
          Error(
            error.TypeError(type_error.TypeError(
              "sum function can only be used on floats, integers, and USD.",
            )),
          )
      }
    }
    expr.BuiltInAvg(key) -> {
      let assert option.Some(key) = key

      // The sum builtin will sum up all the values above it until it finds a label, so we need to fetch those 
      // values and sort them
      let items_above_sum_call =
        grid.to_list(env)
        |> list.filter(fn(i) {
          let #(gk, _) = i
          grid.col(gk) == grid.col(key) && grid.row(gk) < grid.row(key)
        })
        |> list.sort(fn(i1, i2) {
          let #(gk1, _) = i1
          let #(gk2, _) = i2
          int.compare(grid.row(gk1), grid.row(gk2))
        })
        |> list.reverse
        |> list.take_while(fn(i) {
          let #(_, item) = i
          case item {
            Ok(expr.LabelDef(_)) -> False
            _ -> True
          }
        })

      let keys = list.map(items_above_sum_call, fn(i) { i.0 })
      let items_above_sum_call = list.map(items_above_sum_call, fn(i) { i.1 })

      use <- bool.guard(
        list.any(items_above_sum_call, result.is_error),
        Error(
          error.TypeError(type_error.TypeError(
            "Cell above sum expression has error",
          )),
        ),
      )

      let types =
        list.map(items_above_sum_call, fn(i) {
          let assert Ok(item) = i
          item
        })
        |> list.unique
        |> list.map(fn(e) { typecheck(env, e) })

      use <- bool.guard(
        list.any(types, result.is_error),
        Error(
          error.TypeError(type_error.TypeError(
            "Cell above sum expression has type error",
          )),
        ),
      )

      let types =
        list.map(types, fn(t) {
          let assert Ok(texpr) = t
          texpr.type_
        })
        |> list.unique
        |> list.filter(fn(t) { t != typ.TTestResult && t != typ.TNil })

      case types {
        [typ.TFloat] -> Ok(typed_expr.BuiltinAvg(typ.TFloat, keys))
        [typ.TInt] -> Ok(typed_expr.BuiltinAvg(typ.TInt, keys))
        [typ.TUsd] -> Ok(typed_expr.BuiltinAvg(typ.TUsd, keys))
        [typ.TPercent] -> Ok(typed_expr.BuiltinAvg(typ.TPercent, keys))
        _ ->
          Error(
            error.TypeError(type_error.TypeError(
              "avg function can only be used on floats, integers, and USD.",
            )),
          )
      }
    }
    expr.LabelDef(txt) -> {
      // Duplicate label definitions should be a type error
      let defs =
        grid.fold(env, 0, fn(count, _, expr) {
          case expr {
            Ok(expr.LabelDef(t)) if txt == t -> count + 1
            _ -> count
          }
        })

      case defs {
        1 -> Ok(typed_expr.LabelDef(typ.TNil, txt))
        _ if defs > 1 ->
          Error(error.TypeError(type_error.TypeError("Duplicate Label")))
        _ ->
          Error(
            error.TypeError(type_error.TypeError(
              "This is an internal compiler error.",
            )),
          )
      }
    }
    expr.Label(txt) -> {
      let key =
        env
        |> grid.to_list
        |> list.fold_until(None, fn(_, i) {
          case i {
            #(cell_ref, Ok(expr.LabelDef(label_txt))) if label_txt == txt -> {
              Stop(Some(cell_ref))
            }
            _ -> Continue(None)
          }
        })
        |> option.map(grid.cell_to_the_right(env, _))
        |> option.map(option.from_result)
        |> option.flatten

      case key {
        None ->
          Error(
            error.TypeError(type_error.TypeError(
              "Label doesn't point to anything",
            )),
          )
        Some(key) -> {
          let x = grid.get(env, key)
          case x {
            Error(e) -> Error(e)
            Ok(expr.Label(ltxt)) if ltxt == txt -> {
              Error(
                error.TypeError(type_error.TypeError("Label points to itself")),
              )
            }
            Ok(expr) -> {
              case typecheck(env, expr) {
                Ok(te) -> Ok(typed_expr.Label(type_: te.type_, key:, txt:))
                Error(e) -> Error(e)
              }
            }
          }
        }
      }
    }
    expr.CrossLabel(row:, col:) -> {
      let col_cell =
        env
        |> grid.to_list
        |> list.fold_until(None, fn(_, cell) {
          case cell {
            #(cell_ref, Ok(expr.LabelDef(label_txt))) if label_txt == col -> {
              Stop(Some(cell_ref))
            }
            _ -> Continue(None)
          }
        })

      case col_cell {
        None ->
          Error(
            error.TypeError(type_error.TypeError("No label called: " <> col)),
          )
        Some(col_cell) -> {
          let row_cell =
            env
            |> grid.to_list
            |> list.fold_until(None, fn(_, i) {
              case i {
                #(cell_ref, Ok(expr.LabelDef(label_txt))) if label_txt == row -> {
                  Stop(Some(cell_ref))
                }
                _ -> Continue(None)
              }
            })

          case row_cell {
            None ->
              Error(
                error.TypeError(type_error.TypeError("No label called: " <> row)),
              )
            Some(row_cell) -> {
              let new_key = grid.intersect(row_cell, col_cell)
              case new_key {
                Error(_) ->
                  Error(
                    error.TypeError(type_error.TypeError(
                      "Labels " <> row <> " and " <> col <> " do not intersect",
                    )),
                  )
                Ok(nk) -> {
                  let x = grid.get(env, nk)
                  case x {
                    Error(e) -> Error(e)
                    Ok(expr) -> {
                      case typecheck(env, expr) {
                        Ok(te) ->
                          Ok(typed_expr.CrossLabel(
                            type_: te.type_,
                            key: nk,
                            row_label: row,
                            col_label: col,
                          ))
                        Error(e) -> Error(e)
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    expr.BooleanLiteral(b) ->
      Ok(typed_expr.BooleanLiteral(type_: typ.TBool, b:))
    expr.FloatLiteral(f) -> Ok(typed_expr.FloatLiteral(type_: typ.TFloat, f:))
    expr.UsdLiteral(cents) -> Ok(typed_expr.UsdLiteral(type_: typ.TUsd, cents:))
    expr.PercentLiteral(p) -> Ok(typed_expr.PercentLiteral(typ.TPercent, p))
    expr.IntegerLiteral(n) -> Ok(typed_expr.IntegerLiteral(type_: typ.TInt, n:))
    expr.Group(inner) -> {
      use expr <- result.try(typecheck(env, inner))
      Ok(typed_expr.Group(type_: expr.type_, expr:))
    }
    expr.UnaryOp(op, expr) -> {
      use expr <- result.try(typecheck(env, expr))
      case op, expr.type_ {
        expr.Negate, typ.TInt | expr.Negate, typ.TFloat | expr.Negate, typ.TUsd ->
          Ok(typed_expr.UnaryOp(type_: expr.type_, op:, expr:))
        expr.Not, typ.TBool | expr.Not, typ.TPercent ->
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
        // Float x Float
        typ.TFloat, expr.Add, typ.TFloat ->
          Ok(typed_expr.BinaryOp(type_: typ.TFloat, lhs:, op:, rhs:))
        typ.TFloat, expr.Subtract, typ.TFloat ->
          Ok(typed_expr.BinaryOp(type_: typ.TFloat, lhs:, op:, rhs:))
        typ.TFloat, expr.Multiply, typ.TFloat ->
          Ok(typed_expr.BinaryOp(type_: typ.TFloat, lhs:, op:, rhs:))
        typ.TFloat, expr.Divide, typ.TFloat ->
          Ok(typed_expr.BinaryOp(type_: typ.TFloat, lhs:, op:, rhs:))
        typ.TFloat, expr.Power, typ.TFloat ->
          Ok(typed_expr.BinaryOp(type_: typ.TFloat, lhs:, op:, rhs:))
        typ.TFloat, expr.Minimum, typ.TFloat ->
          Ok(typed_expr.BinaryOp(type_: typ.TFloat, lhs:, op:, rhs:))

        // USD x Float (None for now)
        // Percent x Float (None for now)
        // String x Float (None for now)
        // Boolean x Float (None for now)
        // Float x Int
        typ.TFloat, expr.Power, typ.TInt ->
          Ok(typed_expr.BinaryOp(type_: typ.TFloat, lhs:, op:, rhs:))

        // USD x Int
        typ.TUsd, expr.Multiply, typ.TInt ->
          Ok(typed_expr.BinaryOp(type_: typ.TUsd, lhs:, op:, rhs:))
        typ.TUsd, expr.Divide, typ.TInt ->
          Ok(typed_expr.BinaryOp(type_: typ.TUsd, lhs:, op:, rhs:))

        // Percent x Int
        typ.TPercent, expr.Power, typ.TInt ->
          Ok(typed_expr.BinaryOp(type_: typ.TPercent, lhs:, op:, rhs:))

        // String x Int (None for now)
        // Boolean x Int (None for now)
        // Float x Usd (None for now)
        // Usd x Usd
        typ.TUsd, expr.Add, typ.TUsd ->
          Ok(typed_expr.BinaryOp(type_: typ.TUsd, lhs:, op:, rhs:))
        typ.TUsd, expr.Subtract, typ.TUsd ->
          Ok(typed_expr.BinaryOp(type_: typ.TUsd, lhs:, op:, rhs:))
        typ.TUsd, expr.Divide, typ.TUsd ->
          Ok(typed_expr.BinaryOp(type_: typ.TPercent, lhs:, op:, rhs:))
        typ.TUsd, expr.Minimum, typ.TUsd ->
          Ok(typed_expr.BinaryOp(type_: typ.TUsd, lhs:, op:, rhs:))
        typ.TUsd, expr.Multiply, typ.TUsd ->
          Error(error.TypeError(type_error.CannotMultiplyUsdByUsd(lhs:, rhs:)))

        // Percent x Usd
        typ.TPercent, expr.Multiply, typ.TUsd ->
          Ok(typed_expr.BinaryOp(type_: typ.TUsd, lhs:, op:, rhs:))

        // String x Usd (None for now)
        // Boolean x Usd (None for now)
        // Float x Percent (None for now)
        // Usd x Percent
        typ.TUsd, expr.Multiply, typ.TPercent ->
          Ok(typed_expr.BinaryOp(type_: typ.TUsd, lhs:, op:, rhs:))
        typ.TUsd, expr.Divide, typ.TPercent ->
          Ok(typed_expr.BinaryOp(type_: typ.TUsd, lhs:, op:, rhs:))

        // Percent x Percent
        typ.TPercent, expr.Divide, typ.TPercent ->
          Ok(typed_expr.BinaryOp(type_: typ.TPercent, lhs:, op:, rhs:))
        typ.TPercent, expr.Multiply, typ.TPercent ->
          Ok(typed_expr.BinaryOp(type_: typ.TPercent, lhs:, op:, rhs:))
        typ.TPercent, expr.Power, typ.TPercent ->
          Ok(typed_expr.BinaryOp(type_: typ.TPercent, lhs:, op:, rhs:))
        typ.TPercent, expr.Minimum, typ.TPercent ->
          Ok(typed_expr.BinaryOp(type_: typ.TPercent, lhs:, op:, rhs:))

        // String x Percent (None for now)
        // Boolean x Percent (None for now)
        // Float x String (None for now)
        // Usd x String (None for now)
        // Percent x String (None for now)
        // String x String (None for now)
        // Boolean x String (None for now)
        // Float x Boolean (None for now)
        // Usd x Boolean (None for now)
        // Percent x Boolean (None for now)
        // String x Boolean (None for now)
        // Boolean x Boolean
        typ.TBool, expr.And, typ.TBool | typ.TBool, expr.Or, typ.TBool ->
          Ok(typed_expr.BinaryOp(type_: typ.TBool, lhs:, op:, rhs:))

        // Int x Float 
        typ.TInt, expr.Power, typ.TFloat ->
          Ok(typed_expr.BinaryOp(type_: typ.TFloat, lhs:, op:, rhs:))

        // Int x Int
        typ.TInt, expr.Add, typ.TInt ->
          Ok(typed_expr.BinaryOp(type_: typ.TInt, lhs:, op:, rhs:))
        typ.TInt, expr.Subtract, typ.TInt ->
          Ok(typed_expr.BinaryOp(type_: typ.TInt, lhs:, op:, rhs:))
        typ.TInt, expr.Multiply, typ.TInt ->
          Ok(typed_expr.BinaryOp(type_: typ.TInt, lhs:, op:, rhs:))
        typ.TInt, expr.Divide, typ.TInt ->
          Ok(typed_expr.BinaryOp(type_: typ.TInt, lhs:, op:, rhs:))
        typ.TInt, expr.Minimum, typ.TInt ->
          Ok(typed_expr.BinaryOp(type_: typ.TInt, lhs:, op:, rhs:))

        // Int x Usd
        typ.TInt, expr.Multiply, typ.TUsd ->
          Ok(typed_expr.BinaryOp(type_: typ.TUsd, lhs:, op:, rhs:))

        // Int x Percent (Not for now)
        // Int x String (Not for now)
        // Int x Boolean (Not for now)
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
        | typ.TUsd, expr.LessThanCheck, typ.TUsd
        | typ.TUsd, expr.LessThanOrEqualCheck, typ.TUsd
        | typ.TUsd, expr.GreaterThanOrEqualCheck, typ.TUsd
        | typ.TUsd, expr.GreaterThanCheck, typ.TUsd
        | typ.TPercent, expr.LessThanCheck, typ.TPercent
        | typ.TPercent, expr.LessThanOrEqualCheck, typ.TPercent
        | typ.TPercent, expr.GreaterThanOrEqualCheck, typ.TPercent
        | typ.TPercent, expr.GreaterThanCheck, typ.TPercent
        -> Ok(typed_expr.BinaryOp(type_: typ.TBool, lhs:, op:, rhs:))

        // A mustbe op will return the TestResult type, and must be called
        // with the same type on either side of the op
        lht, expr.MustBe, rht if rht == lht ->
          Ok(typed_expr.BinaryOp(type_: typ.TTestResult, lhs:, op:, rhs:))

        lht, binary_op, rht ->
          Error(
            error.TypeError(type_error.IncorrectTypesForBinaryOp(
              lht,
              rht,
              binary_op,
            )),
          )
      }
    }
  }
}
