import gleam/bool
import gleam/float
import gleam/int
import gleam/list
import gleam/result
import squared_away/squared_away_lang/error
import squared_away/squared_away_lang/grid
import squared_away/squared_away_lang/interpreter/runtime_error
import squared_away/squared_away_lang/interpreter/value
import squared_away/squared_away_lang/parser/expr
import squared_away/squared_away_lang/typechecker/typ
import squared_away/squared_away_lang/typechecker/typed_expr
import squared_away/squared_away_lang/util/rational

pub fn interpret(
  env: grid.Grid(Result(typed_expr.TypedExpr, error.CompileError)),
  expr: typed_expr.TypedExpr,
) -> Result(value.Value, error.CompileError) {
  case expr {
    typed_expr.Empty(_) -> Ok(value.Empty)
    typed_expr.LabelDef(_, txt) -> Ok(value.DoNotEvaluate(txt))
    typed_expr.UsdLiteral(_, cents) -> Ok(value.Usd(cents))
    typed_expr.PercentLiteral(_, r) -> Ok(value.Percent(r))
    typed_expr.StringLiteral(_, txt) -> Ok(value.Text(txt))
    typed_expr.Group(_, expr) -> interpret(env, expr)
    typed_expr.CrossLabel(_, key, _, _) -> {
      case grid.get(env, key) {
        Ok(expr) -> interpret(env, expr)
        Error(_) -> Ok(value.Empty)
      }
    }
    typed_expr.Label(_, key, _, txt) -> {
      case grid.get(env, key) {
        Error(e) -> Error(e)
        Ok(te) -> {
          case te {
            typed_expr.Label(_, _, _, ltxt) if ltxt == txt -> {
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
        expr.Not, value.Percent(p) ->
          Ok(value.Percent(rational.subtract(rational.from_int(1), p)))
        _, _ ->
          Error(
            error.RuntimeError(runtime_error.RuntimeError(
              "These should be the only options if the typechecker is working",
            )),
          )
      }
    }
    typed_expr.BinaryOp(_, lhs, op, rhs) -> {
      use lhs <- result.try(interpret(env, lhs))
      use rhs <- result.try(interpret(env, rhs))
      case lhs, op, rhs {
        // Int x Int
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
        value.Integer(a), expr.Minimum, value.Integer(b) ->
          Ok(value.Integer(int.min(a, b)))

        // Float x Float
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
        value.FloatingPointNumber(a), expr.Minimum, value.FloatingPointNumber(b)
        -> Ok(value.FloatingPointNumber(float.min(a, b)))

        value.FloatingPointNumber(base),
          expr.Power,
          value.FloatingPointNumber(exponent)
        -> {
          let fractional_exponent = float.ceiling(exponent) -. exponent >. 0.0
          use <- bool.guard(
            base <. 0.0 && fractional_exponent,
            Error(
              error.RuntimeError(runtime_error.RuntimeError(
                "Cannot raise negative number to fractional power as it produces an imaginary number.",
              )),
            ),
          )

          use <- bool.guard(
            base == 0.0 && exponent <. 0.0,
            Error(
              error.RuntimeError(runtime_error.RuntimeError(
                "Raising 0.0 to a negative power is equivalent to doing division by zero.",
              )),
            ),
          )

          // The above checks are pulled directly from the float.power function. We do them ourselves so we can provide
          // specific error messages.
          let assert Ok(x) = float.power(base, exponent)

          Ok(value.FloatingPointNumber(x))
        }

        // Float x Int
        value.FloatingPointNumber(base), expr.Power, value.Integer(exponent) -> {
          // In our case we can raise a floating point number to an integer power by converting the int to a float

          use <- bool.guard(
            base == 0.0 && exponent < 0,
            Error(
              error.RuntimeError(runtime_error.RuntimeError(
                "
            Raising 0.0 to a negative power is equivalent to doing division by zero.
          ",
              )),
            ),
          )

          // The above checks are pulled directly from the float.power function. We do them ourselves so we can provide
          // specific error messages.
          let assert Ok(x) = float.power(base, exponent |> int.to_float)

          Ok(value.FloatingPointNumber(x))
        }

        // USD x Int
        value.Usd(c), expr.Multiply, value.Integer(i) ->
          Ok(value.Usd(rational.multiply(c, rational.from_int(i))))
        value.Usd(d), expr.Divide, value.Integer(p) -> {
          Ok(value.Usd(rational.divide(d, rational.from_int(p))))
        }

        // Percent x Int
        value.Percent(p), expr.Power, value.Integer(i) -> {
          case rational.power(p, i) {
            Error(_) ->
              Error(
                error.RuntimeError(runtime_error.RuntimeError(
                  "
              Cannot raise 0% to a negative power, it's equivalent to dividing by zero.
            ",
                )),
              )
            Ok(x) -> Ok(value.Percent(x))
          }
        }

        // Usd x Usd
        value.Usd(c1), expr.Add, value.Usd(c2) ->
          Ok(value.Usd(rational.add(c1, c2)))
        value.Usd(c1), expr.Subtract, value.Usd(c2) ->
          Ok(value.Usd(rational.subtract(c1, c2)))
        value.Usd(d1), expr.Divide, value.Usd(d2) ->
          Ok(value.Percent(rational.divide(d1, d2)))
        value.Usd(d), expr.Minimum, value.Usd(p) -> {
          Ok(value.Usd(rational.min(d, p)))
        }

        // Percent x Usd
        value.Percent(p), expr.Multiply, value.Usd(d) -> {
          Ok(value.Usd(rational.multiply(p, d)))
        }

        // Usd x Percent
        value.Usd(c), expr.Multiply, value.Percent(p) -> {
          Ok(value.Usd(rational.multiply(c, p)))
        }
        value.Usd(d), expr.Divide, value.Percent(p) -> {
          Ok(value.Usd(rational.divide(d, p)))
        }

        // Percent x Percent
        value.Percent(p1), expr.Multiply, value.Percent(p2) -> {
          Ok(value.Percent(rational.multiply(p1, p2)))
        }
        value.Percent(p1), expr.Divide, value.Percent(p2) -> {
          Ok(value.Percent(rational.divide(p1, p2)))
        }
        value.Percent(p1), expr.Minimum, value.Percent(p2) -> {
          Ok(value.Percent(rational.min(p1, p2)))
        }
        value.Percent(base), expr.Power, value.Percent(exponent) -> {
          let fractional_exponent = !rational.is_whole_number(exponent)
          use <- bool.guard(
            rational.is_negative(base) && fractional_exponent,
            Error(
              error.RuntimeError(runtime_error.RuntimeError(
                "Cannot raise negative number to fractional power as it produces an imaginary number.",
              )),
            ),
          )

          use <- bool.guard(
            rational.is_zero(base) && rational.is_negative(exponent),
            Error(
              error.RuntimeError(runtime_error.RuntimeError(
                "Raising zero to a negative power is equivalent to doing division by zero.",
              )),
            ),
          )

          Error(
            error.RuntimeError(runtime_error.RuntimeError(
              "Raising a rational number to another rational number power is not implemented yet.",
            )),
          )
        }

        // Bool x Bool
        value.Boolean(a), expr.And, value.Boolean(b) ->
          Ok(value.Boolean(a && b))
        value.Boolean(a), expr.Or, value.Boolean(b) -> Ok(value.Boolean(a || b))
        value.Boolean(a), expr.EqualCheck, value.Boolean(b) ->
          Ok(value.Boolean(a == b))
        value.Boolean(a), expr.NotEqualCheck, value.Boolean(b) ->
          Ok(value.Boolean(a != b))

        // Int x Float 
        value.Integer(base), expr.Power, value.FloatingPointNumber(exponent) -> {
          let fractional_exponent = float.ceiling(exponent) -. exponent >. 0.0
          use <- bool.guard(
            base < 0 && fractional_exponent,
            Error(
              error.RuntimeError(runtime_error.RuntimeError(
                "Cannot raise negative number to fractional power as it produces an imaginary number.",
              )),
            ),
          )

          use <- bool.guard(
            base == 0 && exponent <. 0.0,
            Error(
              error.RuntimeError(runtime_error.RuntimeError(
                "Raising 0.0 to a negative power is equivalent to doing division by zero.",
              )),
            ),
          )

          // The above checks are pulled directly from the float.power function. We do them ourselves so we can provide
          // specific error messages.
          let assert Ok(x) = int.power(base, exponent)

          Ok(value.FloatingPointNumber(x))
        }

        // Int x USD
        value.Integer(i), expr.Multiply, value.Usd(c) ->
          Ok(value.Usd(rational.multiply(c, rational.from_int(i))))

        // MustBe
        vlhs, expr.MustBe, vrhs ->
          case vlhs == vrhs {
            False -> Ok(value.TestFail)
            True -> Ok(value.TestPass)
          }

        lhs, op, rhs -> {
          let msg =
            "these should be the only options if the typechecker is working properly. "
            <> value.value_to_string(lhs)
            <> expr.binary_to_string(op)
            <> value.value_to_string(rhs)
          Error(error.RuntimeError(runtime_error.RuntimeError(msg)))
        }
      }
    }
    typed_expr.BuiltinSum(type_:, keys:) -> {
      let values =
        grid.to_list(env)
        |> list.filter_map(fn(i) {
          let #(gk, item) = i
          case list.contains(keys, gk) {
            False -> Error(Nil)
            True ->
              case item {
                Error(_) -> Error(Nil)
                Ok(x) -> {
                  interpret(env, x) |> result.nil_error
                }
              }
          }
        })
        |> list.filter(fn(v) {
          case v {
            value.TestFail | value.TestPass | value.Empty -> False
            _ -> True
          }
        })

      case type_ {
        typ.TFloat ->
          list.map(values, fn(v) {
            let assert value.FloatingPointNumber(f) = v
            f
          })
          |> float.sum
          |> value.FloatingPointNumber
          |> Ok
        typ.TInt ->
          list.map(values, fn(v) {
            let assert value.Integer(i) = v
            i
          })
          |> int.sum
          |> value.Integer
          |> Ok
        typ.TUsd ->
          list.map(values, fn(v) {
            let assert value.Usd(d) = v
            d
          })
          |> rational.sum
          |> value.Usd
          |> Ok
        _ ->
          Error(
            error.RuntimeError(runtime_error.RuntimeError(
              "internal compiler error sum function interpret",
            )),
          )
      }
    }
    typed_expr.BuiltinAvg(type_:, keys:) -> {
      let values =
        grid.to_list(env)
        |> list.filter_map(fn(i) {
          let #(gk, item) = i
          case list.contains(keys, gk) {
            False -> Error(Nil)
            True ->
              case item {
                Error(_) -> Error(Nil)
                Ok(x) -> {
                  interpret(env, x) |> result.nil_error
                }
              }
          }
        })
        |> list.filter(fn(v) {
          case v {
            value.TestFail | value.TestPass | value.Empty -> False
            _ -> True
          }
        })

      case type_ {
        typ.TFloat -> {
          let sum =
            list.map(values, fn(v) {
              let assert value.FloatingPointNumber(f) = v
              f
            })
            |> float.sum

          Ok(value.FloatingPointNumber(sum /. int.to_float(list.length(values))))
        }

        typ.TInt -> {
          let sum =
            list.map(values, fn(v) {
              let assert value.Integer(i) = v
              i
            })
            |> int.sum

          Ok(value.Integer(sum / list.length(values)))
        }

        typ.TUsd ->
          list.map(values, fn(v) {
            let assert value.Usd(d) = v
            d
          })
          |> rational.avg
          |> value.Usd
          |> Ok
        _ ->
          Error(
            error.RuntimeError(runtime_error.RuntimeError(
              "internal compiler error sum function interpret",
            )),
          )
      }
    }
  }
}
