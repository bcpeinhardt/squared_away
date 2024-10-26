import squared_away_lang/parser/expr
import squared_away_lang/typechecker/typ

pub type TypeError {
  TypeError(context: String)
  IncorrectTypesForBinaryOp(
    lhs: typ.Typ,
    rhs: typ.Typ,
    binary_op: expr.BinaryOpKind,
  )
}

pub fn to_string(te: TypeError) -> String {
  case te {
    IncorrectTypesForBinaryOp(lhs:, rhs:, binary_op:) -> "Type Error:\n
Incorrect arguments for the binary operation.\n
Operation: " <> describe_binary_op_kind_for_err(binary_op) <> "\n
Type on the left side: " <> typ.to_string(lhs) <> "\n
Type on the right side: " <> typ.to_string(rhs)
    TypeError(txt) -> txt
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
  }
}
