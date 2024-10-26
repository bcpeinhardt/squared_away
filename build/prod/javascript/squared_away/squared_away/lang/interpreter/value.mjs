import * as $bool from "../../../../gleam_stdlib/gleam/bool.mjs";
import * as $float from "../../../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../../../gleam_stdlib/gleam/int.mjs";
import { CustomType as $CustomType } from "../../../gleam.mjs";

export class Empty extends $CustomType {}

export class Text extends $CustomType {
  constructor(inner) {
    super();
    this.inner = inner;
  }
}

export class Integer extends $CustomType {
  constructor(n) {
    super();
    this.n = n;
  }
}

export class FloatingPointNumber extends $CustomType {
  constructor(f) {
    super();
    this.f = f;
  }
}

export class Boolean extends $CustomType {
  constructor(b) {
    super();
    this.b = b;
  }
}

export function value_to_string(fv) {
  if (fv instanceof Empty) {
    return "";
  } else if (fv instanceof Text) {
    let t = fv.inner;
    return t;
  } else if (fv instanceof Integer) {
    let n = fv.n;
    return $int.to_string(n);
  } else if (fv instanceof Boolean) {
    let b = fv.b;
    return $bool.to_string(b);
  } else {
    let f = fv.f;
    return $float.to_string(f);
  }
}
