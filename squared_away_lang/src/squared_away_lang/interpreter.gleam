import gleam/float
import gleam/int
import gleam/io
import gleam/list.{Continue, Stop}
import gleam/option.{None, Some}
import gleam/result
import squared_away_lang/error
import squared_away_lang/grid
import squared_away_lang/interpreter/runtime_error
import squared_away_lang/interpreter/value
import squared_away_lang/parser/expr
import squared_away_lang/typechecker/typ
import squared_away_lang/typechecker/typed_expr

pub fn interpret(
  env: grid.Grid(Result(typed_expr.TypedExpr, error.CompileError)),
  expr: typed_expr.TypedExpr,
) -> Result(value.Value, error.CompileError) {
  case expr {
    typed_expr.Empty(_) -> Ok(value.Empty)
    typed_expr.LabelDef(_, txt) -> Ok(value.Text(txt))
    typed_expr.Group(_, expr) -> interpret(env, expr)
    typed_expr.CrossLabel(_, key, _, _) -> {
      case grid.get(env, key) {
        Ok(expr) -> interpret(env, expr)
        Error(_) -> Ok(value.Empty)
      }
    }
    typed_expr.Label(_, txt) -> {
      io.debug(txt)
      let key =
        env
        |> grid.to_list
        |> list.fold_until(None, fn(_, i) {
          case i {
            #(cell_ref, Ok(typed_expr.LabelDef(_, label_txt)))
              if label_txt == txt
            -> {
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
            error.RuntimeError(runtime_error.RuntimeError(
              "Label doesn't point to anything",
            )),
          )
        Some(key) -> {
          case grid.get(env, key) {
            Error(e) -> Error(e)
            Ok(te) -> {
              case te {
                typed_expr.Label(_, ltxt) if ltxt == txt -> {
                  Error(
                    error.RuntimeError(runtime_error.RuntimeError(
                      "Label points to itself",
                    )),
                  )
                }
                _ -> interpret(env, te)
              }
            }
          }
        }
      }
    }
    typed_expr.BooleanLiteral(_, b) -> Ok(value.Boolean(b))
    typed_expr.IntegerLiteral(_, n) -> Ok(value.Integer(n))
    typed_expr.FloatLiteral(_, f) -> Ok(value.FloatingPointNumber(f))
    typed_expr.UnaryOp(_, op, expr) -> {
      use value <- result.try(interpret(env, expr))
      case op, value {
        expr.Negate, value.Integer(n) -> Ok(value.Integer(-n))
        expr.Negate, value.FloatingPointNumber(f) ->
          Ok(value.FloatingPointNumber(float.negate(f)))
        expr.Not, value.Boolean(b) -> Ok(value.Boolean(!b))
        _, _ ->
          panic as "These should be the only options if the typechecker is working"
      }
    }
    typed_expr.BinaryOp(_, lhs, op, rhs) -> {
      use lhs <- result.try(interpret(env, lhs))
      use rhs <- result.try(interpret(env, rhs))
      case lhs, op, rhs {
        // Integer operations
        value.Integer(a), expr.Add, value.Integer(b) -> Ok(value.Integer(a + b))
        value.Integer(a), expr.Subtract, value.Integer(b) ->
          Ok(value.Integer(a - b))
        value.Integer(a), expr.Multiply, value.Integer(b) ->
          Ok(value.Integer(a * b))
        value.Integer(a), expr.Divide, value.Integer(b) ->
          Ok(value.Integer(a / b))
        value.Integer(a), expr.EqualCheck, value.Integer(b) ->
          Ok(value.Boolean(a == b))
        value.Integer(a), expr.NotEqualCheck, value.Integer(b) ->
          Ok(value.Boolean(a != b))
        value.Integer(a), expr.GreaterThanCheck, value.Integer(b) ->
          Ok(value.Boolean(a > b))
        value.Integer(a), expr.GreaterThanOrEqualCheck, value.Integer(b) ->
          Ok(value.Boolean(a >= b))
        value.Integer(a), expr.LessThanCheck, value.Integer(b) ->
          Ok(value.Boolean(a < b))
        value.Integer(a), expr.LessThanOrEqualCheck, value.Integer(b) ->
          Ok(value.Boolean(a <= b))

        // Float operations
        value.FloatingPointNumber(a), expr.Add, value.FloatingPointNumber(b) ->
          Ok(value.FloatingPointNumber(a +. b))
        value.FloatingPointNumber(a),
          expr.Subtract,
          value.FloatingPointNumber(b)
        -> Ok(value.FloatingPointNumber(a -. b))
        value.FloatingPointNumber(a),
          expr.Multiply,
          value.FloatingPointNumber(b)
        -> Ok(value.FloatingPointNumber(a *. b))
        value.FloatingPointNumber(a), expr.Divide, value.FloatingPointNumber(b) ->
          Ok(value.FloatingPointNumber(a /. b))
        value.FloatingPointNumber(a),
          expr.EqualCheck,
          value.FloatingPointNumber(b)
        -> Ok(value.Boolean(a == b))
        value.FloatingPointNumber(a),
          expr.NotEqualCheck,
          value.FloatingPointNumber(b)
        -> Ok(value.Boolean(a != b))
        value.FloatingPointNumber(a),
          expr.GreaterThanCheck,
          value.FloatingPointNumber(b)
        -> Ok(value.Boolean(a >. b))
        value.FloatingPointNumber(a),
          expr.GreaterThanOrEqualCheck,
          value.FloatingPointNumber(b)
        -> Ok(value.Boolean(a >=. b))
        value.FloatingPointNumber(a),
          expr.LessThanCheck,
          value.FloatingPointNumber(b)
        -> Ok(value.Boolean(a <. b))
        value.FloatingPointNumber(a),
          expr.LessThanOrEqualCheck,
          value.FloatingPointNumber(b)
        -> Ok(value.Boolean(a <=. b))

        // Exponents
        value.Integer(a), expr.Power, value.FloatingPointNumber(b) -> {
          let assert Ok(p) = int.power(a, b)
          Ok(value.FloatingPointNumber(p))
        }
        value.FloatingPointNumber(a), expr.Power, value.FloatingPointNumber(b) -> {
          let assert Ok(p) = float.power(a, b)
          Ok(value.FloatingPointNumber(p))
        }

        // Boolean operations
        value.Boolean(a), expr.And, value.Boolean(b) ->
          Ok(value.Boolean(a && b))
        value.Boolean(a), expr.Or, value.Boolean(b) -> Ok(value.Boolean(a || b))
        value.Boolean(a), expr.EqualCheck, value.Boolean(b) ->
          Ok(value.Boolean(a == b))
        value.Boolean(a), expr.NotEqualCheck, value.Boolean(b) ->
          Ok(value.Boolean(a != b))

        lhs, op, rhs -> {
          let msg =
            "these should be the only options if the typechecker is working properly. "
            <> value.value_to_string(lhs)
            <> expr.binary_to_string(op)
            <> value.value_to_string(rhs)
          panic as msg
        }
      }
    }
    // This will get interpreted in the parent grid-aware func to have access to keys.
    typed_expr.BuiltinSum(type_:, keys:) -> {
      // At this point, we've validated that the values to sum are either
      // all ints or all floats, so we'll match on the type and sum them appropriately
      let values =
        grid.to_list(env)
        |> list.filter_map(fn(i) {
          let #(gk, item) = i
          case list.contains(keys, gk) {
            False -> Error(Nil)
            True ->
              case item {
                Error(_) -> Error(Nil)
                Ok(x) -> interpret(env, x) |> result.nil_error
              }
          }
        })

      let val = case type_ {
        typ.TFloat ->
          list.map(values, fn(v) {
            let assert value.FloatingPointNumber(f) = v
            f
          })
          |> float.sum
          |> value.FloatingPointNumber
        typ.TInt ->
          list.map(values, fn(v) {
            let assert value.Integer(i) = v
            i
          })
          |> int.sum
          |> value.Integer
        _ -> panic as "internal compiler error sum function interpret"
      }

      Ok(val)
    }
  }
}
