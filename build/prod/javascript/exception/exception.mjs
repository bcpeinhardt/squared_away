import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import { rescue, defer } from "./exception_ffi.mjs";
import { CustomType as $CustomType } from "./gleam.mjs";

export { defer, rescue };

export class Errored extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Thrown extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Exited extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}
