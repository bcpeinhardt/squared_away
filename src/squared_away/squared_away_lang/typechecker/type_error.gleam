import gleam/option.{None}
import gleam/string
import squared_away/renderable_error
import squared_away/squared_away_lang/parser/expr
import squared_away/squared_away_lang/typechecker/typ
import squared_away/squared_away_lang/typechecker/typed_expr

pub type TypeError {
  TypeError(context: String)
  IncorrectTypesForBinaryOp(
    lhs: typ.Typ,
    rhs: typ.Typ,
    binary_op: expr.BinaryOpKind,
  )
  CannotMultiplyUsdByUsd(lhs: typed_expr.TypedExpr, rhs: typed_expr.TypedExpr)
}

pub fn to_renderable_error(te: TypeError) -> renderable_error.RenderableError {
  case te {
    IncorrectTypesForBinaryOp(lhs, rhs, op) ->
      renderable_error.RenderableError(
        title: "Unexpected arguments to binary operation "
          <> describe_binary_op_kind_for_err(op),
        info: "Got "
          <> typ.to_string(lhs)
          <> " on the left and "
          <> typ.to_string(rhs)
          <> " on the right",
        hint: None,
      )
    TypeError(t) ->
      renderable_error.RenderableError(title: "Type Error", info: t, hint: None)
    CannotMultiplyUsdByUsd(lhs, rhs) -> {
      renderable_error.RenderableError(
        title: "Cannot multiply USD * USD",
        info: "You're multiplying two values that both represent United States Dollars. You have "
          <> string.drop_left(typed_expr.to_string(lhs), 1)
          <> " on the left and "
          <> string.drop_left(typed_expr.to_string(rhs), 1)
          <> " on the right.",
        hint: option.Some(
          "This is *probably* a mistake, as \"dollars squared\" doesn't make much sense as a unit.",
        ),
      )
    }
  }
}

pub fn describe_binary_op_kind_for_err(bo: expr.BinaryOpKind) -> String {
  case bo {
    expr.Add -> "Addition `+`"
    expr.And -> "Boolean And `&&`"
    expr.Divide -> "Division `/`"
    expr.EqualCheck -> "Equality Check `==`"
    expr.GreaterThanCheck -> "Greater Than Check `>`"
    expr.GreaterThanOrEqualCheck -> "Greater Than Or Equal Check `>=`"
    expr.LessThanCheck -> "Less Than Check `<`"
    expr.LessThanOrEqualCheck -> "Less Than or Equal Check `<=`"
    expr.Multiply -> "Multiplication `*`"
    expr.NotEqualCheck -> "Not Equal Check `!=`"
    expr.Or -> "Boolean Or `||`"
    expr.Power -> "To The Power Of `**`"
    expr.Subtract -> "Subtraction `-`"
    expr.MustBe -> "MustBe `mustbe`"
  }
}
