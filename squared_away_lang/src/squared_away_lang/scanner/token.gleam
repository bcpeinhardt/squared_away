pub type Token {
  /// +, addition op for integers
  Plus
  /// -, subtraction op for integers
  Minus
  /// *
  Star
  /// /
  Div
  /// **
  StarStar
  /// =
  Equal
  /// ==
  EqualEqual
  /// !=
  BangEqual
  /// !
  Bang
  /// <
  Less
  /// <=
  LessEqual
  /// >
  Greater
  /// >=
  GreaterEqual
  /// 6, 73
  IntegerLiteral(n: Int)
  /// 1.0, 6.87
  FloatLiteral(f: Float)
  /// True
  TrueToken
  /// False
  FalseToken
  /// &&
  And
  /// ||
  Or
  /// (
  LParen
  /// ) 
  RParen
  Label(String)
  LabelDef(txt: String)
  Underscore
}
