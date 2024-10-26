import { CustomType as $CustomType } from "../../../gleam.mjs";

export class TypeError extends $CustomType {
  constructor(context) {
    super();
    this.context = context;
  }
}
