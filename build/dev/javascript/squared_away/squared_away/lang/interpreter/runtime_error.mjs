import { CustomType as $CustomType } from "../../../gleam.mjs";

export class RuntimeError extends $CustomType {
  constructor(context) {
    super();
    this.context = context;
  }
}
