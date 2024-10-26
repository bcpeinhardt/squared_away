import { CustomType as $CustomType } from "../../../gleam.mjs";

export class Empty extends $CustomType {}

export class FloatLiteral extends $CustomType {
  constructor(f) {
    super();
    this.f = f;
  }
}

export class StringLiteral extends $CustomType {
  constructor(txt) {
    super();
    this.txt = txt;
  }
}

export class IntegerLiteral extends $CustomType {
  constructor(n) {
    super();
    this.n = n;
  }
}

export class CellReference extends $CustomType {
  constructor(key) {
    super();
    this.key = key;
  }
}

export class BooleanLiteral extends $CustomType {
  constructor(val) {
    super();
    this.val = val;
  }
}

export class UnaryOp extends $CustomType {
  constructor(op, expr) {
    super();
    this.op = op;
    this.expr = expr;
  }
}

export class BinaryOp extends $CustomType {
  constructor(lhs, op, rhs) {
    super();
    this.lhs = lhs;
    this.op = op;
    this.rhs = rhs;
  }
}

export class Group extends $CustomType {
  constructor(inner) {
    super();
    this.inner = inner;
  }
}

export class Add extends $CustomType {}

export class Subtract extends $CustomType {}

export class Multiply extends $CustomType {}

export class Divide extends $CustomType {}

export class Power extends $CustomType {}

export class EqualCheck extends $CustomType {}

export class NotEqualCheck extends $CustomType {}

export class LessThanCheck extends $CustomType {}

export class LessThanOrEqualCheck extends $CustomType {}

export class GreaterThanCheck extends $CustomType {}

export class GreaterThanOrEqualCheck extends $CustomType {}

export class And extends $CustomType {}

export class Or extends $CustomType {}

export class Negate extends $CustomType {}

export class Not extends $CustomType {}
