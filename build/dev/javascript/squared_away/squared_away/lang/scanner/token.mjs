import { CustomType as $CustomType } from "../../../gleam.mjs";

export class Plus extends $CustomType {}

export class Minus extends $CustomType {}

export class Star extends $CustomType {}

export class Div extends $CustomType {}

export class StarStar extends $CustomType {}

export class Equal extends $CustomType {}

export class EqualEqual extends $CustomType {}

export class BangEqual extends $CustomType {}

export class Bang extends $CustomType {}

export class Less extends $CustomType {}

export class LessEqual extends $CustomType {}

export class Greater extends $CustomType {}

export class GreaterEqual extends $CustomType {}

export class IntegerLiteral extends $CustomType {
  constructor(n) {
    super();
    this.n = n;
  }
}

export class FloatLiteral extends $CustomType {
  constructor(f) {
    super();
    this.f = f;
  }
}

export class TrueToken extends $CustomType {}

export class FalseToken extends $CustomType {}

export class And extends $CustomType {}

export class Or extends $CustomType {}

export class LParen extends $CustomType {}

export class RParen extends $CustomType {}

export class CellReference extends $CustomType {
  constructor(key) {
    super();
    this.key = key;
  }
}

export class Label extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}
