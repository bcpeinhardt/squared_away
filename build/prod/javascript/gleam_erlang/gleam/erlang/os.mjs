import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";

export class WindowsNt extends $CustomType {}

export class Linux extends $CustomType {}

export class Darwin extends $CustomType {}

export class FreeBsd extends $CustomType {}

export class Other extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}
