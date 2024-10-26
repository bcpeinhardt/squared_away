import { CustomType as $CustomType } from "../../gleam.mjs";
import * as $runtime_error from "../../squared_away/lang/interpreter/runtime_error.mjs";
import * as $parse_error from "../../squared_away/lang/parser/parse_error.mjs";
import * as $scan_error from "../../squared_away/lang/scanner/scan_error.mjs";
import * as $type_error from "../../squared_away/lang/typechecker/type_error.mjs";

export class ScanError extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class ParseError extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class TypeError extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class RuntimeError extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}
