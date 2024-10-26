import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";
import * as $atom from "../gleam/erlang/atom.mjs";
import * as $charlist from "../gleam/erlang/charlist.mjs";

class Safe extends $CustomType {}

export class Eof extends $CustomType {}

export class NoData extends $CustomType {}

export class Second extends $CustomType {}

export class Millisecond extends $CustomType {}

export class Microsecond extends $CustomType {}

export class Nanosecond extends $CustomType {}

export class Exited extends $CustomType {
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

export class Errored extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class UnknownApplication extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}

export class ApplicationFailedToStart extends $CustomType {
  constructor(name, reason) {
    super();
    this.name = name;
    this.reason = reason;
  }
}
